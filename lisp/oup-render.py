#!/usr/bin/env python3
"""Render a paginated Unicode text file (pages split by form feeds) to PDF.

Text is drawn on a fixed monospace grid; characters missing from the main
font fall back to other fonts but keep their grid cell.  A line of the form

    [image:PATH|WxH]

(W, H in points, PATH relative to --base) is replaced by that image,
drawn top-aligned at the line's position and centred in the text width.
The pagination step has already reserved enough blank lines below it.

Usage: oup-render.py IN.txt OUT.pdf [--font-size 10] [--leading 12]
                     [--left 60] [--top 60] [--columns 72] [--base DIR]
"""
import argparse, os, re, subprocess
from reportlab.lib.pagesizes import A4
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.pdfgen import canvas

FONTS = ["DejaVu Sans Mono", "DejaVu Sans", "DejaVu Math TeX Gyre"]
IMAGE = re.compile(r"^\s*\[image:(.+)\|([\d.]+)x([\d.]+)\]\s*$")


def load_fonts():
    fonts = []
    for i, family in enumerate(FONTS):
        path = subprocess.run(["fc-match", "-f", "%{file}", family],
                              capture_output=True, text=True).stdout
        name = f"F{i}"
        pdfmetrics.registerFont(TTFont(name, path))
        fonts.append((name, pdfmetrics.getFont(name).face.charToGlyph))
    return fonts


def runs(line, fonts):
    """Split LINE into (font, start_column, text) runs.
    Only main-font runs are merged: its advance is exactly one cell, while
    fallback glyphs are drawn one per cell to stay on the grid."""
    out = []
    for col, ch in enumerate(line):
        font = next((n for n, cmap in fonts if ord(ch) in cmap), fonts[0][0])
        if (out and font == fonts[0][0] and out[-1][0] == font
                and out[-1][1] + len(out[-1][2]) == col):
            out[-1] = (font, out[-1][1], out[-1][2] + ch)
        else:
            out.append((font, col, ch))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("txt"); ap.add_argument("pdf")
    ap.add_argument("--font-size", type=float, default=10)
    ap.add_argument("--leading", type=float, default=12)
    ap.add_argument("--left", type=float, default=60)
    ap.add_argument("--top", type=float, default=60)
    ap.add_argument("--columns", type=int, default=72)
    ap.add_argument("--base", help="directory image paths are relative to"
                    " (default: that of IN.txt)")
    a = ap.parse_args()

    fonts = load_fonts()
    base = a.base or os.path.dirname(os.path.abspath(a.txt))
    cell = pdfmetrics.stringWidth("M", fonts[0][0], a.font_size)
    text_width = a.columns * cell
    _, page_h = A4

    c = canvas.Canvas(a.pdf, pagesize=A4)
    with open(a.txt, encoding="utf-8") as f:
        pages = f.read().split("\f")
    for page in pages:
        for i, line in enumerate(page.lstrip("\n").split("\n")):
            top = page_h - a.top - i * a.leading          # top of line i
            m = IMAGE.match(line)
            if m:
                path, w, h = m.group(1), float(m.group(2)), float(m.group(3))
                x = a.left + (text_width - w) / 2
                c.drawImage(os.path.join(base, path), x, top - h, w, h,
                            mask="auto")
                continue
            baseline = top - a.font_size                  # cap height ~ size
            for font, col, s in runs(line, fonts):
                c.setFont(font, a.font_size)
                c.drawString(a.left + col * cell, baseline, s)
        c.showPage()
    c.save()


if __name__ == "__main__":
    main()

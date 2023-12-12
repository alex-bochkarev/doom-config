;;; calendars.el -*- lexical-binding: t; -*-

(load! "./calendar-reqs.el")
;; This file contains private info: calendar URL and calendar ID
;; in the form:
;; (setq org-caldav-url "https://caldav-url")
;; (setq org-caldav-calendar-id "calendar-id")

(setq org-export-with-broken-links t)
(setq org-caldav-inbox "~/org/calendar.org")
(setq org-caldav-files '("~/PKB/notes/20231205101723-bo_vqe_milena_roers_bsc.org"
                         "~/PKB/notes/proj-notes/QuanTUK/project.org"
                         "~/PKB/notes/20231026102330-grover_for_sat_project.org"
                         "~/PKB/notes/conferences.org"))

(setq org-icalendar-timezone "Europe/Berlin")
(setq org-caldav-select-tags '("cal"))

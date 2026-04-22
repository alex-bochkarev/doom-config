;;; imaginary-friends.el -*- lexical-binding: t; -*-

(on-host "workbox"
    (setq gptel-model 'qwen2.5:14b
          gptel-backend (gptel-make-ollama "Dell-Ollama"           ; Any name of your choosing
                                           :host "localhost:11434"               ;Where it's running
                                           :stream t                             ;Stream responses
                                           :models '(qwen2.5:14b ;; <- daily
                                                  ; Recommended: Qwen3.5: 14b for hard stuff, but still locally --- 32 Gb upgrade needed
                                                     qwen3.5:9b  ;; <- heavy reasoning
                                                     qwen2.5-coder:7b ;; <- coding
                                                     deepseek-r1:latest ;; <- since I have it :)
                                                     )))
    (gptel-make-deepseek "DeepSeek"
      :stream t
      :host "api.deepseek.com"
      :endpoint "/chat/completions"
      :key (auth-source-pick-first-password :host "api.deepseek.com")
      :models '(deepseek-chat deepseek-reasoner)))

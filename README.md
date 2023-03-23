# Leafy Mode
## Installation

Clone this repo to some folder $FOLDER, get an $OPENAI_API_KEY, then add the following to your .emacs:

```elisp
(defvar leafy-api-key "$OPENAI_API_KEY")
(add-to-list 'load-path "$FOLDER")
(require 'leafy)

(add-hook 'leafy-mode-hook #'leafy-enable-mode-line)
(add-hook 'org-mode-hook #'leafy-mode)
```

Default keybindings are:

```elisp
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'request-completion-at-point)
	    (define-key map (kbd "C-c c") 'leafy-log-context)
            map)
```

## Usage

Write an org-mode document as usual, but you can invoke `request-completion-at-point` at any time to send your document to ChatGPT to request completion. The first time you request a completion, a property drawer will be created that holds numbers like total token counts per model.

If your context fills up, sibling and ancestor nodes are prioritized first and anything else dropped until it gets below the limit.

`leafy-log-context` is used to make a temporary buffer with the context that would be sent to the API, for debugging or for easy pasting to the web UI.

There is a status bar that looks like "Leafy: GPT3.5 | Cost: $0.79". It shows what model you're making API requests to, and how much you've spent so far. You can click the "GPT3.5" part to change the model. See `leafy-model-info-alist` if you want to add new models.

You can add tags to your headlines:
```
* You are ProjectGPT - a project management assistant that can also code :system:
** Some headline :ignore:
* GPT chat
** Introduction :user:
Hello ChatGPT! We are working on an [...]
** ChatGPT Response :assistant:
```
* :ignore: means the branch is always removed from context
* :system: means the Chat Completions API receives that headline as a "system" role.
* :assistant: specifies "assistant" role to the API
* :user: is assumed by default, and specifies "user" role to the API.

## TODO items
If you'd like to contribute, consider working on these:

* Accurate token counting: `leafy-estimate-tokens-regex` is an approximation of OpenAI's tokenization used to prioritize which sections are removed first. Right now, it overcounts and could be made exact. An older version of leafy-mode kept a Python process active and passed strings to it, but it was too error-prone so I asked ChatGPT for a regex replacement that's close enough. You might use OpenAI's Rust tiktoken or a reimplementation and load it via FFI, fix the Python interpreter, or more.
* Code block responses: Instead of creating a sibling section, maybe it could create an org-mode code block to hold the response. The :assistant: tags are used to determine the role sent to the API, but as long as you find some reliable way to find and tag the ChatGPT responses, code blocks might be a preferred way.
* Minifying code before sending: ChatGPT can read minified code, so if there's a general way to minify code(especially in a code block that's been tagged with a specific language) that could help save on tokens. Some early experiments showed up to a 60% token savings, though the more expensive ChatGPT-4 was needed to read the minified code.

Remember to run the tests with `M-x ert-run-tests-interactively` before committing any changes.

## License

GNU GPL v3

# drink-water.el

**Hydration reminder for Emacs. Stay healthy, stay hydrated!**

## Features
- Smart reminders to drink water at calculated intervals based on your weight and day length
- Motivational hydration quotes with every notification
- Customizable cup size, wake/sleep hours, and notification title
- Tracks your daily water intake
- Easy to enable/disable as a global minor mode
- Uses the [alert](https://github.com/jwiegley/alert) package for notifications (cross-platform)

## Installation
1. Copy `drink-water.el` and `drink-water-quotes.el` to a directory in your `load-path`.
2. Install the `alert` package (available on MELPA):
   ```elisp
   M-x package-install RET alert RET
   ```
3. Add the following to your Emacs config:
   ```elisp
   (require 'drink-water)
   ```
4. Optionally, customize variables via `M-x customize-group RET drink-water RET`.

## Usage
- Enable reminders: `M-x drink-water-mode`
- Disable reminders: `M-x drink-water-mode` (toggle off)
- Log that you drank a cup: `M-x drink-water-drank`

Notifications will appear at smart intervals, with a motivational quote.

## Customization
- `drink-water-weight-kg` — Your weight in kg (affects daily water need)
- `drink-water-cup-size-ml` — Size of your cup in ml
- `drink-water-wake-hour` / `drink-water-sleep-hour` — Your day start/end (24h)
- `drink-water-notification-title` — Notification title
- Quotes are stored in the variable `drink-water-quotes` (see `drink-water-quotes.el`)

## Quotes
You can add or edit quotes in `drink-water-quotes.el` by modifying the `drink-water-quotes` variable (a list of strings).

## Testing
Run tests with:
```elisp
ert-run-tests-interactively RET drink-water RET
```

---
*Stay hydrated, stay productive!* 

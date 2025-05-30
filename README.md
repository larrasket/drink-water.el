# drink-water.el

**Hydration reminder for Emacs. Stay healthy, stay hydrated!**

## Features
- Smart reminders to drink water at calculated intervals based on your weight and day length
- Motivational hydration quotes with every notification
- Customizable cup size, wake/sleep hours, notification title, and minimum notification frequency
- Tracks your daily water intake
- Easy to enable/disable as a global minor mode
- Uses the [alert](https://github.com/jwiegley/alert) package for notifications (cross-platform)

## How Notification Frequency Works
The package calculates how often you should be reminded to drink water based on:
- Your weight (`drink-water-weight-kg`)
- Your cup size (`drink-water-cup-size-ml`)
- Your wake and sleep hours (`drink-water-wake-hour`, `drink-water-sleep-hour`)

It divides your day into as many intervals as you need to meet your daily water goal. However, you will **never** be notified more frequently than the value of `drink-water-minimum-interval-minutes` (default: 60 minutes). This prevents excessive notifications.

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

Notifications will appear at smart intervals, with a motivational quote, but never more frequently than your minimum interval.

## Customization
All variables can be customized via `M-x customize-group RET drink-water RET` or by setting them in your config:

- `drink-water-weight-kg` (default: 70)
  - Your weight in kilograms. Used to estimate daily water needs.
- `drink-water-cup-size-ml` (default: 250)
  - The size of your typical cup in milliliters.
- `drink-water-wake-hour` (default: 8)
  - The hour you start your day (24h format).
- `drink-water-sleep-hour` (default: 22)
  - The hour you end your day (24h format).
- `drink-water-notification-title` (default: "💧 Drink Water!")
  - The title for the hydration notification.
- `drink-water-minimum-interval-minutes` (default: 60)
  - The minimum number of minutes between notifications. No notification will be shown more frequently than this interval, even if your calculated interval is shorter.
- Quotes are stored in the variable `drink-water-quotes` (see `drink-water-quotes.el`). You can add or edit quotes by modifying this variable (a list of strings).

## Quotes
You can add or edit quotes in `drink-water-quotes.el` by modifying the `drink-water-quotes` variable (a list of strings).

## Testing
Run tests with:
```elisp
ert-run-tests-interactively RET drink-water RET
```

## License
GPLv3

---
*Stay hydrated, stay productive!* 

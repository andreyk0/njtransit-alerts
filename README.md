## Polls NJT alerts, sends Android FCM push notifications.


```
Polls NJT alerts, sends Android FCM push notifications.

Usage: njtransit-alerts [-v|--verbose] (-l|--nj-line ARG)
                        [-t|--look-back-time-minutes ARG] (-T|--fcm-to ARG)
                        (-k|--fcm-auth-key ARG)
                        [-r|--refresh-interval-minutes ARG]
  Polls NJT alerts, sends Android FCM push notifications.

Available options:
  -h,--help                Show this help text
  -v,--verbose             Be verbose.
  -l,--nj-line ARG         NJT line abbreviation
                           (https://www.njtransit.com/mt/mt_servlet.srv?hdnPageAction=MTNotificationsTo).
  -t,--look-back-time-minutes ARG
                           Discard notifications older than this number of
                           minutes. (default: 60)
  -T,--fcm-to ARG          FCM 'to' (token or topic).
  -k,--fcm-auth-key ARG    FCM auth key. Defaults to FCM_AUTH_KEY env var.
  -r,--refresh-interval-minutes ARG
                           RSS feed refresh interval, minutes. (default: 10)
```

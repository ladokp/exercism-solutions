.moment + "T00:00:00Z"[.moment | length - 10:]
| fromdate + 1e9
| todate
| .[:-1]

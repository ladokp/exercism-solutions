def day( year ; month ; day ): [year, month - 1, day, 0, 0, 0, 0, 0, 0] | mktime;

  . as { $year, $month, $week, $dayofweek }
| [   day( $year ; $month + 1 ; 0 )
    | range( gmtime[2] ) | day( $year ; $month ; . + 1 )
    | select( strftime("%A") == $dayofweek )  ]
| { "first":  first, "second": nth(1), "third":  nth(2)
  , "fourth": nth(3), "last": last,
    "teenth": .[] | select( gmtime[2] | 13 <= . and . <= 19 )  }[ $week ]
| strftime("%Y-%m-%d")
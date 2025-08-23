(.limit + 1) as $limit
| reduce range(2; $limit) as $index1 (
    [range($limit)];
    reduce range($index1 * 2; $limit; $index1) as $index2(.; .[$index2] = false)
  )
| .[2:]
| map(select(. != false))

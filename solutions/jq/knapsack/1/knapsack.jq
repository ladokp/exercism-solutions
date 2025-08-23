def update(item):
   item as {"weight": $weight, "value": $value} | . as $best |
   [range(length)] |
   map(if . >= $weight and $value + $best[. - $weight] > $best[.] then
            $value + $best[. - $weight] else $best[.] end)
;

.maximumWeight as $maximum | .items | reduce .[] as $item ([range($maximum + 1) | 0]; update($item)) | last 
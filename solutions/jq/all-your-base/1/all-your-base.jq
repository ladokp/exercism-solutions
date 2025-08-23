. as { $inputBase, $outputBase }
| select( $inputBase  >= 2 ) // ("input base must be >= 2" | halt_error)
| select( $outputBase >= 2 ) // ("output base must be >= 2" | halt_error )
| select( .digits | all(0 <= . and . < $inputBase) ) // ("all digits must satisfy 0 <= d < input base" | halt_error )
| reduce .digits[] as $d ( 0 ; . * $inputBase + $d )
| [ while( . > 0 ; . / $outputBase | floor ) // 0 | . % $outputBase ]
| reverse

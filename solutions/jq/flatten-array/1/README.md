# Flatten Array

Welcome to Flatten Array on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Introduction

A shipment of emergency supplies has arrived, but there's a problem.
To protect from damage, the items — flashlights, first-aid kits, blankets — are packed inside boxes, and some of those boxes are nested several layers deep inside other boxes!

To be prepared for an emergency, everything must be easily accessible in one box.
Can you unpack all the supplies and place them into a single box, so they're ready when needed most?

## Instructions

Take a nested array of any depth and return a fully flattened array.

Note that some language tracks may include null-like values in the input array, and the way these values are represented varies by track.
Such values should be excluded from the flattened array.

Additionally, the input may be of a different data type and contain different types, depending on the track.

Check the test suite for details.

## Example

input: `[1, [2, 6, null], [[null, [4]], 5]]`

output: `[1, 2, 6, 4, 5]`

## jq-specific Instructions

There is a builtin [`flatten`][flatten] function that largely solves this exercise.
Try to solve this yourself without using the builtin `flatten` function to get the most out of this exercise.

<details><summary>Click here for hints:</summary>

- A recursive function can be useful.
  Learn more about recursion in the [Recursion lesson][recur].
- The [`type`][type] function can help.
</details>

[flatten]: https://jqlang.github.io/jq/manual/v1.7/#flatten
[type]: https://jqlang.github.io/jq/manual/v1.7/#type
[recur]: https://exercism.org/tracks/jq/concepts/recursion

## Source

### Created by

- @MatthijsBlom

### Based on

Interview Question - https://reference.wolfram.com/language/ref/Flatten.html
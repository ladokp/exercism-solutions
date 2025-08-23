def letters: . / "" | sort;

(.subject | ascii_downcase) as $subject
| ($subject | letters) as $letters
| .candidates
| map(select(ascii_downcase | letters == $letters and . != $subject))

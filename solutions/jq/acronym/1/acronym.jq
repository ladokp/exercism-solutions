.phrase
| ascii_upcase
| [ scan("[[:alpha:]][[:alpha:]']*") ]
| map(.[0:1])
| add

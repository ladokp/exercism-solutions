package palindrome

import (
	"fmt"
	"sync"
)

type Product struct {
	Product        int
	Factorizations [][2]int
}

var (
	palindromes []Product
	mtx         = &sync.Mutex{}
	wg          sync.WaitGroup
)

func isPalindrome(p1, p2 int) {
	defer wg.Done()
	numberString := fmt.Sprint(p1 * p2)
	length := len(numberString) - 1
	for index := 0; index <= length; index++ {
		if numberString[index] != numberString[length-index] {
			return
		}
	}
	mtx.Lock()
	addPalindrome(p1, p2)
	mtx.Unlock()
	return
}

func addPalindrome(p1, p2 int) {
	product := p1 * p2
	for index := 0; index < len(palindromes); index++ {
		if palindromes[index].Product == product {
			p := Product{product, append(palindromes[index].Factorizations, [2]int{p1, p2})}
			palindromes[index] = p
			return
		}
	}
	palindromes = append(palindromes, Product{product, [][2]int{{p1, p2}}})
	return
}

func Products(fMin, fMax int) (Product, Product, error) {
	palindromes = []Product{}
	if fMin > fMax {
		return Product{}, Product{}, fmt.Errorf("fmin > fmax")
	}

	for index1 := fMin; index1 <= fMax; index1++ {
		for index2 := index1; index2 <= fMax; index2++ {
			wg.Add(1)
			go isPalindrome(index1, index2)
		}
	}
	wg.Wait()

	if len(palindromes) == 0 {
		return Product{}, Product{}, fmt.Errorf("no palindromes")
	}

	palindromeMin := palindromes[0]
	palindromeMax := palindromes[0]
	for _, palindrome := range palindromes {
		if palindrome.Product > palindromeMax.Product {
			palindromeMax = palindrome
		} else if palindrome.Product < palindromeMin.Product {
			palindromeMin = palindrome
		}
	}
	return palindromeMin, palindromeMax, nil
}

package account

import "sync"

type Account struct {
	sync.RWMutex
	open   bool
	amount int64
}

func Open(amount int64) *Account {
	if amount < 0 {
		return nil
	}
	return &Account{open: true, amount: amount}
}

func (account *Account) Balance() (int64, bool) {
	account.RLock()
	defer account.RUnlock()
	if !account.open {
		return 0, false
	}
	return account.amount, true
}

func (account *Account) Deposit(amount int64) (int64, bool) {
	account.Lock()
	defer account.Unlock()
	if !account.open || account.amount+amount < 0 {
		return 0, false
	}
	account.amount += amount
	return account.amount, true
}

func (account *Account) Close() (int64, bool) {
	account.Lock()
	defer account.Unlock()
	if !account.open {
		return 0, false
	}
	account.open = false
	return account.amount, true
}

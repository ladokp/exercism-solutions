%% Bank Account module using gen_server behaviour.
-module(bank_account).
-behaviour(gen_server).

%% Exporting client functions and server callbacks
-export([balance/1, charge/2, close/1, create/0, deposit/2, withdraw/2]).
-export([init/1, handle_call/3, handle_cast/2]).

%% Record definition for the account, with default state 'open' and balance of 0.
-record(account, {state = open, balance = 0}).

%% Client API functions

%% Creates a new bank account (starts the gen_server process).
create() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

%% Returns the balance of the account by making a synchronous call to the server.
balance(Pid) ->
    gen_server:call(Pid, balance).

%% Charges a specified amount from the account (synchronous call).
charge(Pid, Amount) ->
    gen_server:call(Pid, {charge, Amount}).

%% Closes the bank account (synchronous call).
close(Pid) ->
    gen_server:call(Pid, close).

%% Deposits a specified amount into the account (asynchronous call).
deposit(Pid, Amount) ->
    gen_server:cast(Pid, {deposit, Amount}).

%% Withdraws a specified amount from the account (synchronous call).
withdraw(Pid, Amount) ->
    gen_server:call(Pid, {withdraw, Amount}).

%% Server Callbacks

%% Initializes the server with an account record.
init([]) ->
    {ok, #account{}}.

%% Handle synchronous (call) messages

%% If the account is closed, return an error.
handle_call(_, _, Account) when Account#account.state == closed ->
    {reply, {error, account_closed}, Account};

%% Handle closing the account. Return the current balance and set state to closed.
handle_call(close, _From, Account) ->
    {reply, Account#account.balance, Account#account{state = closed}};

%% Handle balance inquiry. Return the current balance.
handle_call(balance, _From, Account) ->
    {reply, Account#account.balance, Account};

%% Handle withdrawal requests.
handle_call({withdraw, Amount}, _From, Account) ->
    Balance = Account#account.balance,
    if
        Amount < 0 ->  %% Reject negative withdrawals.
            {reply, 0, Account};
        Amount > Balance ->  %% If withdrawal exceeds balance, withdraw all funds.
            {reply, Balance, Account#account{balance = 0}};
        true ->  %% Otherwise, deduct the amount from the balance.
            {reply, Amount, Account#account{balance = Balance - Amount}}
    end;

%% Handle charge requests (similar to withdrawal).
handle_call({charge, Amount}, _From, Account) ->
    Balance = Account#account.balance,
    if
        Amount < 0 ->  %% Reject negative charges.
            {reply, 0, Account};
        Amount > Balance ->  %% Reject if charge exceeds balance.
            {reply, 0, Account};
        true ->  %% Deduct the charge from the balance.
            {reply, Amount, Account#account{balance = Balance - Amount}}
    end.

%% Handle asynchronous (cast) messages

%% If the account is closed, ignore the message.
handle_cast(_, Account) when Account#account.state == closed ->
    {noreply, Account};

%% Handle deposit requests.
handle_cast({deposit, Amount}, Account) ->
    if
        Amount < 0 ->  %% Ignore negative deposits.
            {noreply, Account};
        true ->  %% Add the deposit to the current balance.
            {noreply, Account#account{balance = Account#account.balance + Amount}}
    end.

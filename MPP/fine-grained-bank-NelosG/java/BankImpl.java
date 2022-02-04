import java.util.Arrays;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Stream;
/**
 * Bank implementation.
 *
 * @author Pushkarev Gleb
 */
public class BankImpl implements Bank {
    /**
     * An array of accounts by index.
     */
    private final Account[] accounts;

    /**
     * Creates new bank instance.
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public BankImpl(int n) {
        accounts = new Account[n];
        for (int i = 0; i < n; i++) {
            accounts[i] = new Account();
        }
    }

    @Override
    public int getNumberOfAccounts() {
        return accounts.length;
    }


    @Override
    public long getAmount(int index) {
        accounts[index].lock.lock();
        long res = accounts[index].amount;
        accounts[index].lock.unlock();
        return res;
    }

    @Override
    public long getTotalAmount() {
        long sum = 0;
        Arrays.stream(accounts).forEach(acc -> acc.lock.lock());
        for (Account account : accounts) {
            sum += account.amount;
        }
        Arrays.stream(accounts).forEach(acc -> acc.lock.unlock());
        return sum;
    }


    @Override
    public long deposit(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        Account account = accounts[index];
        try {
            account.lock.lock();
            if (amount > MAX_AMOUNT || account.amount + amount > MAX_AMOUNT)
                throw new IllegalStateException("Overflow");
            account.amount += amount;
            return account.amount;
        } finally {
            account.lock.unlock();
        }
    }

    @Override
    public long withdraw(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        Account account = accounts[index];
        try {
            account.lock.lock();
            if (account.amount - amount < 0)
                throw new IllegalStateException("Underflow");
            account.amount -= amount;
            return account.amount;
        } finally {
            account.lock.unlock();
        }
    }

    @Override
    public void transfer(int fromIndex, int toIndex, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        if (fromIndex == toIndex)
            throw new IllegalArgumentException("fromIndex == toIndex");
        Account from = accounts[fromIndex];
        Account to = accounts[toIndex];
        boolean changeOrder = fromIndex > toIndex;
        try {
            lockOrUnlock(from, to, changeOrder, true);
            if (amount > from.amount)
                throw new IllegalStateException("Underflow");
            else if (amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT)
                throw new IllegalStateException("Overflow");
            from.amount -= amount;
            to.amount += amount;
        } finally {
            lockOrUnlock(from, to, changeOrder, false);
        }
    }

    /**
     * Private account data structure.
     */
    static class Account {
        /**
         * Amount of funds in this account.
         */
        long amount;

        ReentrantLock lock = new ReentrantLock();
    }

    private void lockOrUnlock(Account fromAccount, Account toAccount, boolean changeOrder, boolean toLock) {
        Stream<Account> stream;
        if (changeOrder) stream = Stream.of(toAccount, fromAccount);
        else stream = Stream.of(fromAccount, toAccount);

        if (toLock) stream.forEach(acc -> acc.lock.lock());
        else stream.forEach(acc -> acc.lock.unlock());
    }
}

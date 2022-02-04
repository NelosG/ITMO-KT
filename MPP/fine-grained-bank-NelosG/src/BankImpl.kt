/**
 * Bank implementation.
 *
 * :TODO: This implementation has to be made thread-safe.
 *
 * @author :TODO: LastName FirstName
 */
class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun getAmount(index: Int): Long {
        return accounts[index].amount
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override val totalAmount: Long
        get() {
            return accounts.sumOf { account ->
                account.amount
            }
        }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        check(!(amount > Bank.MAX_AMOUNT || account.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
        account.amount += amount
        return account.amount
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun withdraw(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        check(account.amount - amount >= 0) { "Underflow" }
        account.amount -= amount
        return account.amount
    }

    /**
     * :TODO: This method has to be made thread-safe.
     */
    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }
        val from = accounts[fromIndex]
        val to = accounts[toIndex]
        check(amount <= from.amount) { "Underflow" }
        check(!(amount > Bank.MAX_AMOUNT || to.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
        from.amount -= amount
        to.amount += amount
    }

    /**
     * Private account data structure.
     */
    class Account {
        /**
         * Amount of funds in this account.
         */
        var amount: Long = 0
    }
}
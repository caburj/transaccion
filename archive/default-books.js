const transaction00 = {
    "id": "00",
    "price": 13.4,
    "category": "Food",
    "description": "quality food"
};

const transaction01 = {
    "id": "01",
    "price": 1.4,
    "category": "Misc",
    "description": "toothpaste"
};

const defaultTransactions = {
    "00": transaction00,
    "01": transaction01
};

const book00 = {
    "id": "00",
    "name": "Personal",
    "expenseCategories": ["Uncategorized", "Food", "Rent", "Transpo", "Leisure", "Misc", "Subscription", "Medical", "Unexpected"],
    "earningCategories": ["Uncategorized", "Salary", "Bonus", "Gift", "Reimbursement", "Sideline", "Unexpected"],
    "transactions": defaultTransactions
};

const book01 = {
    "id": "01",
    "name": "Business",
    "expenseCategories": ["Uncategorized", "Food", "Rent", "Transpo", "Leisure", "Misc", "Subscription", "Medical", "Unexpected"],
    "earningCategories": ["Uncategorized", "Salary", "Bonus", "Gift", "Reimbursement", "Sideline", "Unexpected"],
    "transactions": defaultTransactions
};

export default {
    "00": book00,
    "01": book01
};

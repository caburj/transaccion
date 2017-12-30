export default function (folder) {
    return {
        name: folder,
        builder: function (privateClient) {
            privateClient.declareType('book', {
                "type": "object",
                "properties": {
                    "id": {
                        "type": "string"
                    },
                    "name": {
                        "type": "string"
                    },
                    "expenseCategories": {
                        "type": "array",
                        "items": "string"
                    },
                    "earningCategories": {
                        "type": "array",
                        "items": "string"
                    },
                    "transactions": {
                        "type": "object",
                        "additionalProperties": {
                            "type": "object",
                            "properties": {
                                "id": {
                                    "type": "string"
                                },
                                "price": {
                                    "type": "number",
                                    "format": "double"
                                },
                                "category": {
                                    "type": "string"
                                },
                                "description": {
                                    "type": "string"
                                },
                                "created": {
                                    "type": "number",
                                    "format": "double"
                                },
                                "lastEdited": {
                                    "type": "number",
                                    "format": "double"
                                }
                                
                            }
                        }
                    },
                    "created": {
                        "type": "number",
                        "format": "double"
                    },
                    "lastEdited": {
                        "type": "number",
                        "format": "double"
                    }
                }
            });

            return {
                exports: {

                    init: function () {
                        privateClient.cache('');
                    },

                    on: privateClient.on,

                    add: function (book) {
                        let id = book.id;
                        return privateClient.storeObject('book', id, book);
                    },

                    remove: privateClient.remove.bind(privateClient),

                    list: function () {
                        return privateClient.getAll('');
                    }
                }
            }
        }
    };
}
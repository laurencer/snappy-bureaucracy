namespace java au.com.cba.omnia.bureaucracy.experiments.v1.changes

// The Changes Domain
// ==================
//
// Represents changes in a source database system.
//

union ChangedValue {
    1: string unknownValue
    2: string stringValue
    3: i64 longValue
    4: double doubleValue
}

struct ChangedCell {
    1: string columnName
    2: bool hasBeforeValue // whether the source system included a before value.
    // optional to represent nullable values
    3: optional ChangedValue before
    4: optional ChangedValue after
}

enum OperationType {
    Insert = 1,
    Delete = 2,
    Update = 3
}

struct ChangeRecord {
    1: string timestamp   // millis since epoch.
    2: string tableName ( partition = "yes" )
    3: OperationType opType
    4: list<ChangedCell> changes
}


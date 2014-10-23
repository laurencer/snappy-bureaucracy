namespace java au.com.cba.omnia.bureaucracy.data.v1

struct Message {
    1: i64 timestamp;           // millis since epoch in UTC (time that the message was received)
    2: optional string unique;  // unique message id to detect duplicate sends
    3: string source;           // source system (including version)
    4: string user;             // authenticated user who sent the message
    5: string handler;          // handling system (e.g. bureaucracy)
    6: string type;             // type of the message
    7: binary payload;          // actual message data
}

struct FirehoseMessage {
    1: i64 offset;
    2: Message message;
}
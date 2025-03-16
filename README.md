# 💬 Chat Application Using Erlang

![Chat Application](https://via.placeholder.com/1000x400?text=Chat+Application+Using+Erlang)

## 📌 Overview
A scalable chat application built in **Erlang**, leveraging in-memory data structures and **Mnesia DBMS** for storing messages, chat history, and offline messages. The application evolves in multiple versions with enhanced features.

---

## 🚀 Versions

### ✅ Version 1: Basic Chat System
- Implemented using **Erlang** with in-memory data structures for storing messages and history.

### ✅ Version 2: Persistent Storage
- Integrated **Mnesia DBMS** for storing messages, history, and offline messages.

### ⏳ Version 3: Upcoming (OTP Gen_Server)
- Upgrading to **OTP Gen_Server** for improved concurrency and fault tolerance.

---

## 🏗 Tech Stack
![Tech Stack](https://via.placeholder.com/600x300?text=Tech+Stack)
| Component  | Technology Used |
|------------|----------------|
| Backend    | Erlang |
| Database   | Mnesia DBMS |
| Concurrency | OTP (Upcoming) |

---

## ⚡ App Start Commands

### 🖥️ Server Terminal
```sh
# Start Server Terminal with name "server"
erl -sname server   

# Compile server and client files
c(server).
c(client).  

# Start Server with Max Users Allowed Count
server:start(Server_Name, 0, MaxUsersAllowedCount).
```

### 🖥️ Client Terminal
```sh
# Start Client Terminal with name "client"
erl -sname client   

# Connect Client Terminal to Server Terminal
net_kernel:connect_node(Server_Name).  

# Compile server and client files
c(server).  
c(client).

# Start Client with a username
client:start(Server_Name, UserName).
```

---

## 🛠 App Functionality Commands

### 🔹 Server Commands
| Command | Description |
|---------|-------------|
| `server:get_history(Server_Name).` | Get chat history |
| `server:get_connected_clients(Server_Name).` | Get list of connected clients |
| `server:send_message_to_all_clients(Server_Name, "Hello Everyone").` | Broadcast message to all clients |
| `server:make_admin(Server_Name, UserName).` | Assign admin privileges to a user |

### 🔹 Client Commands
| Command | Description |
|---------|-------------|
| `client:send_message(Server_Name, UserName, "hi").` | Send message to chat room |
| `client:exit(Server_Name, UserName).` | Exit chat room |
| `client:send_private_message(Server_Name, ReceiverUserName, SenderUserName, "hi").` | Send private message |
| `client:get_connected_clients(Server_Name, UserName).` | Get list of connected clients |
| `client:update_chat_topic(Server_Name, UserName, "New Topic").` | Update chat room topic |
| `client:get_current_chat_topic(Server_Name, UserName).` | Get current chat room topic |
| `client:perform_admin_action(Server_Name, AdminUserName, UserName, mute, 5).` | Mute/unmute/kick/make admin |
| `client:get_admins(Server_Name, UserName).` | Get list of admins |
| `client:set_status(Server_Name, UserName, offline).` | Update user status |

---

## 🔗 Future Enhancements
![Enhancements](https://via.placeholder.com/600x300?text=Future+Enhancements)
- ✅ Implement **OTP Gen_Server** for better fault tolerance
- ✅ Improve **message persistence** for scalability
- ✅ Introduce **user authentication** and **encryption** for security
- ✅ Build a **web interface** for easier interactions

---

## 🤝 Contributing
![Contributing](https://via.placeholder.com/600x300?text=Contributing)
We welcome contributions! Feel free to **fork, open issues, and submit pull requests**.

---

## 📜 License
![License](https://via.placeholder.com/600x300?text=License)
This project is licensed under the **MIT License**.

💡 *Built for learning, optimized for performance!* 🚀


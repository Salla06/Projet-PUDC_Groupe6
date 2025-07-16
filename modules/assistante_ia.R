library(shiny)
library(readxl)
library(httr)
library(jsonlite)

# ============================================================================
# FONCTIONS ASSISTANT IA - DANS APP.R
# ============================================================================

assistant_ia_ui <- function() {
  fluidPage(
    # CSS et JavaScript personnalisés
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML("
        /* Styles globaux */
        .main-container {
          min-height: 100vh;
          background: #f9fafb;
          font-family: 'Inter', sans-serif;
        }
        
        /* Header styles */
        .header-section {
          background: white;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
          border-bottom: 1px solid #e5e7eb;
          padding: 24px 0;
        }
        
        .status-indicator {
          width: 8px;
          height: 8px;
          border-radius: 50%;
          display: inline-block;
          margin-right: 8px;
        }
        
        .status-connected { background: #10b981; }
        .status-local { background: #f59e0b; }
        
        /* Chat container */
        .chat-container {
          height: 600px;
          background: white;
          border-radius: 12px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.05);
          display: flex;
          flex-direction: column;
          overflow: hidden;
        }
        
        .chat-messages {
          flex: 1;
          overflow-y: auto;
          padding: 20px;
          background: #f9fafb;
        }
        
        .message {
          margin-bottom: 16px;
          display: flex;
          align-items: flex-start;
          animation: slideIn 0.3s ease-out;
        }
        
        .message.user {
          justify-content: flex-end;
        }
        
        .message.assistant {
          justify-content: flex-start;
        }
        
        .message-content {
          max-width: 80%;
          padding: 12px 16px;
          border-radius: 18px;
          position: relative;
          word-wrap: break-word;
          white-space: pre-wrap;
        }
        
        .message.user .message-content {
          background: #3b82f6;
          color: white;
        }
        
        .message.assistant .message-content {
          background: #f3f4f6;
          color: #1f2937;
        }
        
        .message-avatar {
          width: 32px;
          height: 32px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          margin: 0 10px;
          font-size: 14px;
        }
        
        .message.user .message-avatar {
          background: #3b82f6;
          color: white;
        }
        
        .message.assistant .message-avatar {
          background: #6b7280;
          color: white;
        }
        
        .message-time {
          font-size: 11px;
          color: #6b7280;
          margin-top: 4px;
        }
        
        /* Input area */
        .chat-input {
          border-top: 1px solid #e5e7eb;
          padding: 16px;
          background: white;
        }
        
        .input-group {
          display: flex;
          gap: 12px;
          align-items: flex-end;
        }
        
        .message-input {
          flex: 1;
          resize: none;
          border: 1px solid #d1d5db;
          border-radius: 12px;
          padding: 12px 16px;
          font-size: 14px;
          line-height: 1.5;
          transition: all 0.2s;
        }
        
        .send-button {
          padding: 12px 20px;
          background: #3b82f6;
          color: white;
          border: none;
          border-radius: 12px;
          font-weight: 500;
          cursor: pointer;
          transition: all 0.2s;
        }
        
        /* Sidebar styles */
        .sidebar-section {
          background: white;
          border-radius: 12px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.05);
          margin-bottom: 24px;
          overflow: hidden;
        }
        
        .sidebar-header {
          padding: 16px 20px;
          background: #f8fafc;
          border-bottom: 1px solid #e5e7eb;
          font-weight: 600;
          color: #1f2937;
          display: flex;
          align-items: center;
          gap: 8px;
        }
        
        .sidebar-content {
          padding: 20px;
        }
        
        .suggestion-btn {
          width: 100%;
          text-align: left;
          padding: 10px 12px;
          background: #f9fafb;
          border: 1px solid #e5e7eb;
          border-radius: 8px;
          margin-bottom: 6px;
          cursor: pointer;
          transition: all 0.2s;
          font-size: 13px;
          color: #374151;
        }
        
        .suggestion-btn:hover {
          background: #eff6ff;
          border-color: #3b82f6;
          color: #1d4ed8;
        }
        
        /* Loading animation */
        .typing-indicator {
          display: flex;
          align-items: center;
          gap: 4px;
          padding: 12px 16px;
        }
        
        .typing-dot {
          width: 8px;
          height: 8px;
          border-radius: 50%;
          background: #6b7280;
          animation: typing 1.4s infinite;
        }
        
        @keyframes typing {
          0%, 60%, 100% { transform: translateY(0); }
          30% { transform: translateY(-10px); }
        }
        
        @keyframes slideIn {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 1; transform: translateY(0); }
        }
      "))
    ),
    
    # Container principal
    div(class = "main-container",
        # Header
        div(class = "header-section",
            div(class = "container-fluid",
                fluidRow(
                  column(8,
                         h1("Assistant IA PTBA 2025", 
                            style = "color: #1f2937; font-size: 2rem; font-weight: 700; margin: 0;"),
                         p("Votre assistant intelligent pour l'analyse des données", 
                           style = "color: #6b7280; font-size: 1.1rem; margin: 8px 0 16px 0;"),
                         uiOutput("ai_status_indicator")
                  ),
                  column(4,
                         div(style = "text-align: right; padding-top: 8px;",
                             actionButton("clear_chat", "Effacer", 
                                          class = "btn btn-outline-secondary",
                                          icon = icon("trash"))
                         )
                  )
                )
            )
        ),
        
        # Contenu principal
        div(class = "container-fluid", style = "padding: 32px 0;",
            fluidRow(
              # Zone de chat principale
              column(8,
                     div(class = "chat-container",
                         # Messages
                         div(class = "chat-messages", id = "chat-messages",
                             uiOutput("chat_messages_display")
                         ),
                         
                         # Zone de saisie
                         div(class = "chat-input",
                             div(class = "input-group",
                                 textAreaInput("user_message", NULL,
                                               placeholder = "Posez votre question sur les données PTBA 2025...",
                                               rows = 2,
                                               width = "100%"),
                                 actionButton("send_message", "Envoyer", 
                                              class = "send-button",
                                              icon = icon("paper-plane"))
                             )
                         )
                     )
              ),
              
              # Sidebar
              column(4,
                     # Questions suggérées
                     div(class = "sidebar-section",
                         div(class = "sidebar-header",
                             tags$i(class = "fas fa-lightbulb", style = "color: #f59e0b;"),
                             "Questions Suggérées"
                         ),
                         div(class = "sidebar-content",
                             uiOutput("suggested_questions_display")
                         )
                     ),
                     
                     # Historique
                     div(class = "sidebar-section",
                         div(class = "sidebar-header",
                             tags$i(class = "fas fa-history", style = "color: #3b82f6;"),
                             "Historique"
                         ),
                         div(class = "sidebar-content", style = "max-height: 250px; overflow-y: auto;",
                             uiOutput("conversation_history_display")
                         )
                     ),
                     
                     # Statut des fichiers
                     div(class = "sidebar-section",
                         div(class = "sidebar-header",
                             tags$i(class = "fas fa-database", style = "color: #8b5cf6;"),
                             "Données Excel"
                         ),
                         div(class = "sidebar-content",
                             uiOutput("files_status_display")
                         )
                     )
              )
            )
        )
    )
  )
}
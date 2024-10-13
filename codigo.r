# Instalar e carregar os pacotes necessários
if (!require("RSQLite")) {
  install.packages("RSQLite")
}
if (!require("readxl")) {
  install.packages("readxl")
}

library(RSQLite)
library(readxl)

# Conexão com o banco de dados
conectar_bd <- function() {
  conn <- dbConnect(SQLite(), dbname = "solos.db")
  return(conn)
}

# Criar tabela no BD, se não existir
criar_tabela <- function(conn) {
  query <- "CREATE TABLE IF NOT EXISTS solo (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            nome TEXT NOT NULL,
            ph REAL NOT NULL,
            fertilidade TEXT NOT NULL)"
  dbExecute(conn, query)
}

# Função para ler um arquivo Excel (.xlsx)
ler_arquivo_xlsx <- function(caminho_arquivo) {
  df <- read_excel(caminho_arquivo)
  return(df)
}

# Função para adicionar dados de solo a partir de um DataFrame
adicionar_solos_de_arquivo <- function(conn, df) {
  for (i in 1:nrow(df)) {
    nome <- as.character(df$nome[i])
    ph <- as.numeric(df$ph[i])
    fertilidade <- as.character(df$fertilidade[i])
    adicionar_solo(conn, nome, ph, fertilidade)
  }
  cat("Todos os solos do arquivo foram adicionados.\n")
}

# Função para adicionar dados de solo manualmente
adicionar_solo <- function(conn, nome, ph, fertilidade) {
  query <- "INSERT INTO solo (nome, ph, fertilidade) VALUES (?, ?, ?)"
  dbExecute(conn, query, params = list(nome, ph, fertilidade))
  cat("Solo adicionado com sucesso!\n")
}

# Função para exibir informações de um solo
exibir_solo <- function(conn, nome) {
  query <- "SELECT * FROM solo WHERE nome = ?"
  resultado <- dbGetQuery(conn, query, params = list(nome))
  if (nrow(resultado) == 0) {
    cat("Solo não encontrado.\n")
  } else {
    print(resultado)
  }
}

# Função para atualizar informações de um solo
atualizar_solo <- function(conn, nome, novo_ph, nova_fertilidade) {
  query <- "UPDATE solo SET ph = ?, fertilidade = ? WHERE nome = ?"
  dbExecute(conn, query, params = list(novo_ph, nova_fertilidade, nome))
  cat("Solo atualizado com sucesso!\n")
}

# Função para deletar informações de um solo
deletar_solo <- function(conn, nome) {
  query <- "DELETE FROM solo WHERE nome = ?"
  dbExecute(conn, query, params = list(nome))
  cat("Solo deletado com sucesso!\n")
}

# Função para avaliação do solo para plantio de soja ou milho
avaliar_solo <- function(conn, nome) {
  query <- "SELECT * FROM solo WHERE nome = ?"
  resultado <- dbGetQuery(conn, query, params = list(nome))

  if (nrow(resultado) == 0) {
    cat("Solo não encontrado.\n")
  } else {
    ph <- resultado$ph
    fertilidade <- resultado$fertilidade

    # Avaliação simplificada
    if (ph >= 5.8 && ph <= 7.0 && fertilidade == "alta") {
      cat("Solo apto para plantio de soja.\n")
    } else if (ph >= 5.5 && ph <= 7.0 && fertilidade == "alta") {
      cat("Solo apto para plantio de milho.\n")
    } else {
      cat("Solo não apto para plantio de soja ou milho.\n")
    }
  }
}

# Função para exibir o menu
menu <- function() {
  cat("Escolha uma opção:\n")
  cat("1: Adicionar solo manualmente\n")
  cat("2: Exibir solo\n")
  cat("3: Atualizar solo\n")
  cat("4: Deletar solo\n")
  cat("5: Avaliar solo\n")
  cat("6: Adicionar solos de arquivo .xlsx\n")
  cat("0: Sair\n")
}

# Loop principal
main <- function() {
  conn <- conectar_bd()
  criar_tabela(conn)

  repeat {
    menu()
    opcao <- as.integer(readline(prompt = "Digite a opção: "))

    if (opcao == 0) {
      cat("Saindo...\n")
      break
    }

    if (opcao == 1) {
      nome <- readline(prompt = "Nome do solo: ")
      ph <- as.numeric(readline(prompt = "pH do solo: "))
      fertilidade <- readline(prompt = "Fertilidade (baixa/media/alta): ")
      adicionar_solo(conn, nome, ph, fertilidade)

    } else if (opcao == 2) {
      nome <- readline(prompt = "Nome do solo: ")
      exibir_solo(conn, nome)

    } else if (opcao == 3) {
      nome <- readline(prompt = "Nome do solo a atualizar: ")
      novo_ph <- as.numeric(readline(prompt = "Novo pH: "))
      nv_fertilidade <- readline(prompt = "Nova fertilidade(baixa/media/alta):")
      atualizar_solo(conn, nome, novo_ph, nv_fertilidade)

    } else if (opcao == 4) {
      nome <- readline(prompt = "Nome do solo a deletar: ")
      deletar_solo(conn, nome)

    } else if (opcao == 5) {
      nome <- readline(prompt = "Nome do solo: ")
      avaliar_solo(conn, nome)

    } else if (opcao == 6) {
      caminho_arquivo <- readline(prompt = "Digite o caminho do arquivo .xlsx:")
      df <- ler_arquivo_xlsx(caminho_arquivo)
      adicionar_solos_de_arquivo(conn, df)

    } else {
      cat("Opção inválida. Tente novamente.\n")
    }
  }

  dbDisconnect(conn)
}

# Executar o programa
main()

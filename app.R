library(shiny)

# =========================================================
# StatSkripsi: aplikasi eksplorasi data kuantitatif dan kualitatif
# Analisis yang disediakan:
# - audit kualitas data
# - statistik deskriptif numerik
# - frekuensi variabel kategorik
# - korelasi Pearson / Spearman
# - regresi linear sederhana
# - ekspor tabel hasil ke CSV
#
# Catatan:
# Hasil merupakan bantuan analisis, bukan pengganti keputusan
# metodologis peneliti. Periksa definisi variabel, unit observasi,
# desain penelitian, asumsi, dan sumber data sebelum menyimpulkan.
# =========================================================

required_packages <- c("shiny", "readxl")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Paket belum terpasang: ",
    paste(missing_packages, collapse = ", "),
    ". Jalankan install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")) di Console R."
  )
}

library(shiny)

read_dataset <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))

  if (ext == "csv") {
    # Membaca CSV dengan asumsi umum. Jika pemisah/desimal berbeda,
    # simpan ulang sebagai CSV standar atau impor ulang setelah konversi.
    out <- tryCatch(
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA", "N/A", "NULL")
      ),
      error = function(e) {
        stop("CSV gagal dibaca: ", conditionMessage(e))
      }
    )
  } else if (ext %in% c("xls", "xlsx")) {
    out <- tryCatch(
      as.data.frame(readxl::read_excel(path, na = c("", "NA", "N/A", "NULL"))),
      error = function(e) {
        stop("Excel gagal dibaca: ", conditionMessage(e))
      }
    )
  } else {
    stop("Format file tidak didukung. Gunakan CSV, XLS, atau XLSX.")
  }

  if (!is.data.frame(out) || nrow(out) == 0 || ncol(out) == 0) {
    stop("File tidak berisi tabel dengan baris dan kolom yang dapat dianalisis.")
  }

  # Jangan mengubah nama kolom secara diam-diam.
  names(out) <- make.unique(names(out), sep = "_")
  out
}

is_numeric_column <- function(x) {
  is.numeric(x) && !is.logical(x)
}

default_variable_roles <- function(data) {
  numeric_names <- names(data)[vapply(data, is_numeric_column, logical(1))]
  list(
    quantitative = numeric_names,
    qualitative = setdiff(names(data), numeric_names)
  )
}

convert_quantitative_columns <- function(data, vars) {
  for (nm in vars) {
    x <- data[[nm]]
    if (is_numeric_column(x)) next

    raw <- trimws(as.character(x))
    converted <- suppressWarnings(as.numeric(raw))
    invalid <- !is.na(raw) & nzchar(raw) & is.na(converted)
    if (any(invalid)) {
      examples <- paste(utils::head(unique(raw[invalid]), 3), collapse = ", ")
      stop(
        "Kolom '", nm, "' dipilih sebagai kuantitatif tetapi memuat nilai bukan angka (",
        examples, "). Bersihkan nilainya atau ubah peran kolom menjadi kualitatif."
      )
    }
    data[[nm]] <- converted
  }
  data
}

format_number <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}

make_descriptive_table <- function(data, vars) {
  rows <- lapply(vars, function(nm) {
    x <- data[[nm]]
    if (!is_numeric_column(x)) return(NULL)

    valid <- x[is.finite(x)]
    n <- length(valid)

    data.frame(
      Variabel = nm,
      N_valid = n,
      N_missing = sum(is.na(x) | !is.finite(x)),
      Mean = if (n > 0) mean(valid) else NA_real_,
      Median = if (n > 0) median(valid) else NA_real_,
      SD_sampel = if (n > 1) stats::sd(valid) else NA_real_,
      Minimum = if (n > 0) min(valid) else NA_real_,
      Q1 = if (n > 0) unname(stats::quantile(valid, 0.25)) else NA_real_,
      Q3 = if (n > 0) unname(stats::quantile(valid, 0.75)) else NA_real_,
      Maksimum = if (n > 0) max(valid) else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame(Pesan = "Tidak ada variabel numerik yang dipilih."))
  do.call(rbind, rows)
}

audit_data <- function(data) {
  n <- nrow(data)
  p <- ncol(data)
  missing_cells <- sum(is.na(data))
  missing_by_col <- vapply(data, function(x) sum(is.na(x)), numeric(1))
  duplicate_rows <- sum(duplicated(data))
  constant_cols <- names(data)[vapply(data, function(x) {
    vals <- unique(x[!is.na(x)])
    length(vals) <= 1
  }, logical(1))]
  type_names <- vapply(data, function(x) paste(class(x), collapse = "/"), character(1))

  list(
    summary = data.frame(
      Pemeriksaan = c(
        "Jumlah observasi (baris)",
        "Jumlah variabel (kolom)",
        "Jumlah sel kosong/NA",
        "Jumlah baris duplikat persis",
        "Jumlah kolom konstan atau seluruhnya kosong"
      ),
      Hasil = c(
        as.character(n),
        as.character(p),
        as.character(missing_cells),
        as.character(duplicate_rows),
        as.character(length(constant_cols))
      ),
      check.names = FALSE
    ),
    missing = data.frame(
      Variabel = names(data),
      Tipe_data = unname(type_names),
      Nilai_kosong = unname(missing_by_col),
      Persen_kosong = round(100 * unname(missing_by_col) / n, 2),
      check.names = FALSE
    ),
    constant = constant_cols
  )
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("1. Masukkan data"),
      fileInput(
        "file",
        "Unggah CSV atau Excel",
        accept = c(".csv", ".xls", ".xlsx")
      ),
      helpText("Unggah satu file tabel. Untuk CSV, gunakan pemisah koma dan titik sebagai desimal."),
      actionButton("load_demo", "Gunakan data contoh"),
      hr(),
      h4("2. Sesuaikan tipe variabel"),
      helpText("Peran kualitatif dideteksi otomatis dan dapat diubah. Pilih kolom angka berkode kategori di sini; kolom yang tidak dipilih akan diperlakukan sebagai kuantitatif."),
      selectizeInput(
        "qualitative_vars",
        "Tandai kolom sebagai kualitatif",
        choices = character(0),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Cari dan pilih kolom kualitatif")
      ),
      hr(),
      h4("3. Pilih analisis"),
      selectizeInput(
        "numeric_vars",
        "Variabel untuk statistik deskriptif",
        choices = character(0),
        multiple = TRUE,
        options = list(plugins = list("remove_button"), placeholder = "Pilih satu atau beberapa variabel")
      ),
      selectInput("categorical_var", "Variabel untuk tabel frekuensi", choices = character(0)),
      hr(),
      h4("Korelasi"),
      selectInput("corr_x", "Variabel X", choices = character(0)),
      selectInput("corr_y", "Variabel Y", choices = character(0)),
      selectInput(
        "corr_method",
        "Metode korelasi",
        choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
        selected = "pearson"
      ),
      hr(),
      h4("Regresi linear sederhana"),
      selectInput("reg_y", "Variabel hasil (Y)", choices = character(0)),
      selectInput("reg_x", "Variabel prediktor (X)", choices = character(0)),
      helpText("Hasil diperbarui otomatis saat pilihan analisis berubah."),
      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Ringkasan",
          br(),
          h4("Status data"),
          verbatimTextOutput("data_status"),
          h4("Pratinjau data"),
          tableOutput("preview")
        ),
        tabPanel(
          "Audit kualitas",
          br(),
          h4("Ringkasan pemeriksaan"),
          tableOutput("audit_summary"),
          h4("Nilai kosong per variabel"),
          tableOutput("audit_missing"),
          verbatimTextOutput("audit_notes")
        ),
        tabPanel(
          "Deskriptif",
          br(),
          p("SD dihitung sebagai simpangan baku sampel. Nilai kosong tidak dimasukkan ke statistik numerik."),
          tableOutput("descriptive_table"),
          plotOutput("descriptive_plot", height = "360px"),
          downloadButton("download_descriptive", "Unduh tabel deskriptif CSV")
        ),
        tabPanel(
          "Frekuensi",
          br(),
          uiOutput("frequency_heading"),
          tableOutput("frequency_table"),
          plotOutput("frequency_plot", height = "360px"),
          downloadButton("download_frequency", "Unduh tabel frekuensi CSV")
        ),
        tabPanel(
          "Korelasi",
          br(),
          tableOutput("correlation_table"),
          h4("Interpretasi otomatis"),
          verbatimTextOutput("correlation_interpretation"),
          p("Korelasi menunjukkan asosiasi, bukan bukti sebab-akibat. Nilai p tidak mengukur besar atau pentingnya hubungan.")
        ),
        tabPanel(
          "Regresi",
          br(),
          h4("Koefisien model"),
          tableOutput("regression_table"),
          h4("Ringkasan model"),
          tableOutput("regression_fit"),
          h4("Interpretasi otomatis"),
          verbatimTextOutput("regression_interpretation"),
          h4("Diagnostik residual sederhana"),
          plotOutput("residual_plot", height = "320px"),
          p("Regresi linear mengasumsikan hubungan rata-rata linear. Periksa pola residual, pencilan, independensi, dan kesesuaian desain penelitian.")
        ),
        tabPanel(
          "Bab IV",
          br(),
          h4("Draf ringkasan hasil"),
          verbatimTextOutput("chapter4_text"),
          downloadButton("download_chapter4", "Unduh draf TXT")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  source_label <- reactiveVal("Belum ada data")

  observeEvent(dataset(), {
    dat <- dataset()
    req(dat)
    roles <- default_variable_roles(dat)
    updateSelectizeInput(
      session, "qualitative_vars",
      choices = names(dat), selected = roles$qualitative
    )
  }, ignoreInit = TRUE)

  observeEvent(input$load_demo, {
    demo <- data.frame(
      Tahun = 2019:2026,
      Anggaran = c(120, 125, 130, 128, 140, 148, 155, 162),
      Capaian = c(68, 70, 72, 71, 76, 79, 81, 84),
      Kategori = c("A", "A", "B", "A", "B", "B", "C", "C")
    )
    dataset(demo)
    source_label("Data contoh bawaan aplikasi")
  })

  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      dat <- read_dataset(input$file$datapath, input$file$name)
      dataset(dat)
      source_label(input$file$name)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 10)
    })
  })

  variable_roles <- reactive({
    dat <- dataset()
    req(dat)
    qualitative_names <- intersect(input$qualitative_vars, names(dat))
    list(
      quantitative = setdiff(names(dat), qualitative_names),
      qualitative = qualitative_names
    )
  })

  observeEvent(list(dataset(), input$qualitative_vars), {
    dat <- dataset()
    all_names <- if (is.null(dat)) character(0) else names(dat)
    qualitative_names <- intersect(input$qualitative_vars, all_names)
    quantitative_names <- setdiff(all_names, qualitative_names)

    selected_descriptive <- intersect(input$numeric_vars, quantitative_names)
    if (length(selected_descriptive) == 0) selected_descriptive <- quantitative_names
    updateSelectizeInput(
      session, "numeric_vars",
      choices = quantitative_names, selected = selected_descriptive, server = TRUE
    )

    selected_category <- if (length(input$categorical_var) == 1 &&
                             input$categorical_var %in% qualitative_names) {
      input$categorical_var
    } else if (length(qualitative_names) > 0) {
      qualitative_names[1]
    } else {
      character(0)
    }
    updateSelectInput(
      session, "categorical_var",
      choices = qualitative_names, selected = selected_category
    )

    choose_current <- function(current, choices, fallback) {
      if (length(current) == 1 && current %in% choices) return(current)
      if (length(choices) >= fallback) return(choices[fallback])
      character(0)
    }
    choose_pair <- function(x_current, y_current, choices) {
      x <- choose_current(x_current, choices, 1)
      y <- choose_current(y_current, choices, 2)
      if (length(choices) >= 2 && identical(x, y)) {
        y <- setdiff(choices, x)[1]
      }
      list(x = x, y = y)
    }
    correlation_pair <- choose_pair(input$corr_x, input$corr_y, quantitative_names)
    regression_pair <- choose_pair(input$reg_x, input$reg_y, quantitative_names)
    updateSelectInput(
      session, "corr_x", choices = quantitative_names,
      selected = correlation_pair$x
    )
    updateSelectInput(
      session, "corr_y", choices = quantitative_names,
      selected = correlation_pair$y
    )
    updateSelectInput(
      session, "reg_x", choices = quantitative_names,
      selected = regression_pair$x
    )
    updateSelectInput(
      session, "reg_y", choices = quantitative_names,
      selected = regression_pair$y
    )
  })

  analysis_dataset <- reactive({
    dat <- dataset()
    req(dat)
    roles <- variable_roles()
    quantitative_names <- roles$quantitative
    qualitative_names <- roles$qualitative
    dat <- convert_quantitative_columns(dat, quantitative_names)
    for (nm in qualitative_names) dat[[nm]] <- as.character(dat[[nm]])
    dat
  })

  output$data_status <- renderText({
    dat <- dataset()
    if (is.null(dat)) return("Belum ada data. Unggah file atau pilih data contoh.")
    paste0(
      "Sumber: ", source_label(), "\n",
      "Observasi: ", nrow(dat), "\n",
      "Variabel: ", ncol(dat), "\n",
      "Peran variabel saat ini: ",
      length(variable_roles()$quantitative), " kuantitatif, ",
      length(variable_roles()$qualitative), " kualitatif.\n",
      "Catatan: pastikan satu baris merepresentasikan unit observasi yang benar."
    )
  })

  output$preview <- renderTable({
    req(dataset())
    head(dataset(), 10)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$audit_summary <- renderTable({
    req(dataset())
    audit_data(dataset())$summary
  }, striped = TRUE, bordered = TRUE)

  output$audit_missing <- renderTable({
    req(dataset())
    audit_data(dataset())$missing
  }, striped = TRUE, bordered = TRUE)

  output$audit_notes <- renderText({
    req(dataset())
    a <- audit_data(dataset())
    notes <- character(0)

    if (sum(a$missing$Nilai_kosong) > 0) {
      notes <- c(notes, "- Terdapat nilai kosong. Periksa apakah pola kehilangan data dapat memengaruhi analisis.")
    } else {
      notes <- c(notes, "- Tidak ditemukan NA eksplisit pada tabel yang dibaca. Ini tidak menjamin semua data valid.")
    }

    if (length(a$constant) > 0) {
      notes <- c(notes, paste0("- Kolom konstan/seluruhnya kosong: ", paste(a$constant, collapse = ", ")))
    }

    if (sum(duplicated(dataset())) > 0) {
      notes <- c(notes, "- Ada baris duplikat persis. Jangan menghapus otomatis: pastikan apakah duplikat merupakan kesalahan atau observasi yang sah.")
    }

    paste(notes, collapse = "\n")
  })

  descriptive_data <- reactive({
    dat <- analysis_dataset()
    vars <- intersect(input$numeric_vars, variable_roles()$quantitative)
    validate(need(length(vars) > 0, "Pilih setidaknya satu variabel numerik."))
    make_descriptive_table(dat, vars)
  })

  output$descriptive_table <- renderTable({
    descriptive_data()
  }, striped = TRUE, bordered = TRUE, digits = 3)

  output$descriptive_plot <- renderPlot({
    dat <- analysis_dataset()
    vars <- intersect(input$numeric_vars, variable_roles()$quantitative)
    validate(need(length(vars) > 0, "Pilih variabel numerik untuk membuat grafik."))
    vars <- vars[vars %in% names(dat)]
    validate(need(length(vars) > 0, "Variabel yang dipilih tidak tersedia."))

    x <- dat[[vars[1]]]
    x <- x[is.finite(x)]
    validate(need(length(x) > 0, "Tidak ada nilai numerik valid untuk grafik."))

    hist(x, main = paste("Distribusi", vars[1]), xlab = vars[1],
         col = "grey85", border = "white")
  })

  frequency_data <- reactive({
    dat <- analysis_dataset()
    req(input$categorical_var)
    validate(need(input$categorical_var %in% variable_roles()$qualitative,
                  "Pilih variabel yang ditetapkan sebagai kualitatif."))
    x <- dat[[input$categorical_var]]
    x <- as.character(x)
    x[is.na(x) | trimws(x) == ""] <- "(Kosong)"
    tab <- table(x, useNA = "no")
    data.frame(
      Kategori = names(tab),
      Frekuensi = as.integer(tab),
      Persentase = round(100 * as.integer(tab) / sum(tab), 2),
      check.names = FALSE
    )
  })

  output$frequency_heading <- renderUI({
    req(input$categorical_var)
    h4(paste("Distribusi:", input$categorical_var))
  })

  output$frequency_table <- renderTable({
    frequency_data()
  }, striped = TRUE, bordered = TRUE)

  output$frequency_plot <- renderPlot({
    fd <- frequency_data()
    validate(need(nrow(fd) > 0, "Tidak ada kategori untuk ditampilkan."))
    oldpar <- par(mar = c(8, 4, 3, 1))
    on.exit(par(oldpar))
    barplot(fd$Frekuensi, names.arg = fd$Kategori, las = 2,
            main = "Frekuensi per Kategori", ylab = "Frekuensi",
            col = "grey85", border = "white")
  })

  correlation_result <- reactive({
    req(dataset(), input$corr_x, input$corr_y)
    validate(need(input$corr_x != input$corr_y, "Pilih dua variabel yang berbeda."))
    dat <- analysis_dataset()
    x <- dat[[input$corr_x]]
    y <- dat[[input$corr_y]]
    validate(need(is_numeric_column(x) && is_numeric_column(y),
                  "Korelasi hanya tersedia untuk dua variabel kuantitatif."))
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]

    validate(need(length(x) >= 3, "Diperlukan setidaknya 3 pasangan observasi valid."))
    validate(need(length(unique(x)) > 1 && length(unique(y)) > 1,
                  "Korelasi tidak dapat dihitung jika salah satu variabel konstan."))

    method <- input$corr_method
    test <- suppressWarnings(cor.test(x, y, method = method, exact = FALSE))
    estimate <- unname(test$estimate)

    list(
      table = data.frame(
        Variabel_X = input$corr_x,
        Variabel_Y = input$corr_y,
        Metode = if (method == "pearson") "Pearson" else "Spearman",
        N_pasangan = length(x),
        Koefisien = estimate,
        P_value = test$p.value,
        stringsAsFactors = FALSE
      ),
      estimate = estimate,
      p = test$p.value,
      n = length(x),
      method = method,
      xname = input$corr_x,
      yname = input$corr_y
    )
  })

  output$correlation_table <- renderTable({
    r <- correlation_result()
    out <- r$table
    out$Koefisien <- round(out$Koefisien, 4)
    out$P_value <- format.pval(out$P_value, digits = 4, eps = 0.0001)
    out
  }, striped = TRUE, bordered = TRUE)

  output$correlation_interpretation <- renderText({
    r <- correlation_result()
    strength <- if (abs(r$estimate) < 0.10) {
      "sangat lemah atau mendekati nol"
    } else if (abs(r$estimate) < 0.30) {
      "lemah"
    } else if (abs(r$estimate) < 0.50) {
      "sedang"
    } else if (abs(r$estimate) < 0.70) {
      "cukup kuat"
    } else {
      "kuat"
    }

    direction <- if (r$estimate > 0) "positif" else "negatif"
    significance <- if (r$p < 0.05) {
      "Nilai p berada di bawah 0,05; hasil ini signifikan pada ambang tersebut, dengan asumsi pengujian dan desain sampel sesuai."
    } else {
      "Nilai p tidak berada di bawah 0,05; data ini belum memberikan bukti statistik yang cukup untuk menolak hipotesis nol pada ambang 0,05."
    }

    paste0(
      "Draf interpretasi: Analisis korelasi ", if (r$method == "pearson") "Pearson" else "Spearman",
      " antara ", r$xname, " dan ", r$yname, " menghasilkan koefisien ",
      sprintf("%.3f", r$estimate), " dengan ", r$n, " pasangan observasi valid. ",
      "Arah hubungan ", direction, " dan kekuatannya secara deskriptif ", strength, ". ",
      "Nilai p = ", format.pval(r$p, digits = 4, eps = 0.0001), ". ",
      significance, " Kategori kekuatan hanyalah pedoman deskriptif, bukan batas universal. ",
      "Korelasi tidak membuktikan sebab-akibat."
    )
  })

  regression_result <- reactive({
    req(dataset(), input$reg_x, input$reg_y)
    validate(need(input$reg_x != input$reg_y, "Pilih variabel X dan Y yang berbeda."))

    dat <- analysis_dataset()
    x <- dat[[input$reg_x]]
    y <- dat[[input$reg_y]]
    validate(need(is_numeric_column(x) && is_numeric_column(y),
                  "Regresi linear hanya tersedia untuk variabel kuantitatif."))
    ok <- is.finite(x) & is.finite(y)
    clean <- data.frame(x = x[ok], y = y[ok])

    validate(need(nrow(clean) >= 3, "Diperlukan setidaknya 3 pasangan observasi valid."))
    validate(need(length(unique(clean$x)) > 1, "Variabel X konstan; regresi tidak dapat dihitung."))
    validate(need(length(unique(clean$y)) > 1, "Variabel Y konstan; regresi tidak dapat dihitung."))

    model <- lm(y ~ x, data = clean)
    sm <- summary(model)
    coefmat <- as.data.frame(sm$coefficients)
    coefmat$Parameter <- rownames(coefmat)
    rownames(coefmat) <- NULL
    names(coefmat) <- c("Koefisien", "SE", "t", "P_value", "Parameter")
    coefmat <- coefmat[, c("Parameter", "Koefisien", "SE", "t", "P_value")]

    fit <- data.frame(
      Ukuran = c("N", "R-squared", "R-squared terkoreksi", "F-statistic", "P model"),
      Nilai = c(
        as.character(nrow(clean)),
        sprintf("%.4f", sm$r.squared),
        sprintf("%.4f", sm$adj.r.squared),
        sprintf("%.4f", unname(sm$fstatistic[1])),
        format.pval(pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3],
                       lower.tail = FALSE), digits = 4, eps = 0.0001)
      )
    )

    list(
      model = model,
      coefficients = coefmat,
      fit = fit,
      n = nrow(clean),
      xname = input$reg_x,
      yname = input$reg_y,
      r2 = sm$r.squared,
      slope = unname(coef(model)[2]),
      intercept = unname(coef(model)[1]),
      p_slope = coefmat$P_value[coefmat$Parameter == "x"]
    )
  })

  output$regression_table <- renderTable({
    r <- regression_result()
    out <- r$coefficients
    out$Koefisien <- round(out$Koefisien, 4)
    out$SE <- round(out$SE, 4)
    out$t <- round(out$t, 4)
    out$P_value <- format.pval(out$P_value, digits = 4, eps = 0.0001)
    out$Parameter[out$Parameter == "(Intercept)"] <- "Intersep"
    out$Parameter[out$Parameter == "x"] <- "Prediktor X"
    out
  }, striped = TRUE, bordered = TRUE)

  output$regression_fit <- renderTable({
    regression_result()$fit
  }, striped = TRUE, bordered = TRUE)

  output$regression_interpretation <- renderText({
    r <- regression_result()
    dir <- if (r$slope > 0) "positif" else if (r$slope < 0) "negatif" else "nol"
    p <- r$p_slope

    significance <- if (p < 0.05) {
      "Koefisien kemiringan signifikan pada ambang 0,05."
    } else {
      "Koefisien kemiringan tidak signifikan pada ambang 0,05."
    }

    paste0(
      "Draf interpretasi: Model regresi linear sederhana menggunakan ", r$n,
      " observasi valid untuk mengestimasi hubungan ", r$xname, " (X) dengan ",
      r$yname, " (Y). Persamaan estimasi: Y = ",
      sprintf("%.3f", r$intercept), " + (", sprintf("%.3f", r$slope), " × X). ",
      "Koefisien X bertanda ", dir, ". Secara matematis, kenaikan satu unit X berkaitan dengan perubahan rata-rata ",
      sprintf("%.3f", r$slope), " unit pada Y dalam model ini. R-squared = ",
      sprintf("%.3f", r$r2), ", yaitu proporsi variasi Y yang dijelaskan model pada data ini. ",
      "Nilai p koefisien X = ", format.pval(p, digits = 4, eps = 0.0001), ". ",
      significance, " Interpretasi ini tidak membuktikan kausalitas dan perlu dilengkapi pemeriksaan residual, pencilan, independensi, serta konteks penelitian."
    )
  })

  output$residual_plot <- renderPlot({
    r <- regression_result()
    oldpar <- par(mfrow = c(1, 2))
    on.exit(par(oldpar))
    plot(fitted(r$model), resid(r$model),
         xlab = "Nilai prediksi", ylab = "Residual",
         main = "Residual vs Prediksi")
    abline(h = 0, lty = 2)
    qqnorm(resid(r$model), main = "Q-Q Plot Residual")
    qqline(resid(r$model))
  })

  output$chapter4_text <- renderText({
    req(dataset())
    dat <- dataset()
    desc <- tryCatch(descriptive_data(), error = function(e) NULL)
    freq <- tryCatch(frequency_data(), error = function(e) NULL)

    lines <- c(
      "DRAF BAB IV — HASIL DAN PEMBAHASAN",
      "",
      "1. Gambaran data",
      paste0("Dataset yang dianalisis memuat ", nrow(dat), " observasi dan ", ncol(dat), " variabel. Sumber file: ", source_label(), "."),
      paste0("Jumlah sel kosong/NA yang terdeteksi: ", sum(is.na(dat)), ". Baris duplikat persis: ", sum(duplicated(dat)), "."),
      "",
      "2. Statistik deskriptif",
      if (!is.null(desc)) paste(capture.output(print(desc, row.names = FALSE)), collapse = "\n")
      else "Pilih satu atau lebih variabel numerik pada panel kiri untuk membuat tabel deskriptif.",
      "",
      "3. Distribusi variabel kualitatif",
      if (!is.null(freq)) paste(capture.output(print(freq, row.names = FALSE)), collapse = "\n")
      else "Pilih satu variabel kualitatif pada panel kiri untuk membuat tabel frekuensi.",
      "",
      "4. Catatan interpretasi",
      "- Tinjau definisi operasional, unit observasi, periode, sumber, dan proses pengumpulan data.",
      "- Periksa data hilang, duplikat, pencilan, perubahan definisi indikator, serta konsistensi antarperiode.",
      "- Hasil statistik menunjukkan pola pada data yang dianalisis; generalisasi bergantung pada desain dan cakupan data.",
      "- Jangan mengartikan korelasi atau regresi observasional sebagai bukti sebab-akibat tanpa desain dan asumsi yang mendukung.",
      "- Tambahkan sitasi sumber data dan metode analisis yang digunakan sebelum memasukkan draf ke skripsi.",
      "",
      "Catatan: teks ini adalah draf otomatis. Peneliti bertanggung jawab memverifikasi hasil dan menyesuaikannya dengan rumusan masalah."
    )

    paste(lines, collapse = "\n")
  })

  output$download_descriptive <- downloadHandler(
    filename = function() paste0("statistik-deskriptif-", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(descriptive_data(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$download_frequency <- downloadHandler(
    filename = function() paste0("frekuensi-", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(frequency_data(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$download_chapter4 <- downloadHandler(
    filename = function() paste0("draf-bab-IV-", Sys.Date(), ".txt"),
    content = function(file) {
      writeLines(output_text_for_download(), file, useBytes = TRUE)
    }
  )

  output_text_for_download <- reactive({
    # Render the same generated chapter text from the server-side reactive.
    dat <- dataset()
    req(dat)
    desc <- tryCatch(descriptive_data(), error = function(e) NULL)
    freq <- tryCatch(frequency_data(), error = function(e) NULL)
    lines <- c(
      "DRAF BAB IV — HASIL DAN PEMBAHASAN",
      "",
      paste0("Dataset memuat ", nrow(dat), " observasi dan ", ncol(dat), " variabel. Sumber file: ", source_label(), "."),
      paste0("Sel kosong/NA: ", sum(is.na(dat)), ". Baris duplikat persis: ", sum(duplicated(dat)), "."),
      "",
      "STATISTIK DESKRIPTIF",
      if (!is.null(desc)) paste(capture.output(print(desc, row.names = FALSE)), collapse = "\n")
      else "Belum ada variabel numerik yang dipilih.",
      "",
      "DISTRIBUSI VARIABEL KUALITATIF",
      if (!is.null(freq)) paste(capture.output(print(freq, row.names = FALSE)), collapse = "\n")
      else "Belum ada variabel kualitatif yang dipilih.",
      "",
      "CATATAN METODOLOGIS",
      "- Verifikasi unit observasi, definisi variabel, periode, dan sumber data.",
      "- Periksa missing data, duplikat, pencilan, serta asumsi metode.",
      "- Hubungan statistik tidak otomatis menunjukkan sebab-akibat.",
      "- Teks ini merupakan draf yang harus diverifikasi peneliti."
    )
    paste(lines, collapse = "\n")
  })
}

shinyApp(ui = ui, server = server)

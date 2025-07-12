library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(sf)
library(fmsb)
library(readxl)
library(tseries)
library(ggplot2)
library(lmtest)
library(car)
library(kableExtra)
library(formattable)
library(rsconnect)



server <- function(input, output, session) {
  # Menu Home
  observeEvent(input$to_cluster, {
    updateTabItems(session, "tabs", "clustering")
  })
  
  observeEvent(input$to_forecast, {
    updateTabItems(session, "tabs", "forecast_emisi")
  })
  
  observeEvent(input$to_energy, {
    updateTabItems(session, "tabs", "konversi")
  })
  
  
  
  output$sampahBox <- renderValueBox({
    valueBox(
      value = tags$div("112 Juta Ton", style = "font-size: 28px; font-weight: bold; color: white;"),
      subtitle = tags$div("Sampah Makanan per Tahun", style = "font-size: 16px;"),
      icon = icon("trash", class = "fa-solid"),
      color = "purple"
    )
  })
  
  output$listrikBox <- renderValueBox({
    valueBox(
      value = tags$div("474 MWh setiap 750 ton", style = "font-size: 28px; font-weight: bold; color: white;"),
      subtitle = tags$div("Potensi Energi dari FLW", style = "font-size: 16px;"),
      icon = icon("bolt", class = "fa-solid"),
      color = "green"
    )
  })
  
  output$emisiBox <- renderValueBox({
    valueBox(
      value = tags$div("150 Juta Ton CO₂e", style = "font-size: 28px; font-weight: bold; color: white;"),
      subtitle = tags$div("Emisi GRK akibat FLW", style = "font-size: 16px;"),
      icon = icon("cloud", class = "fa-solid"),
      color = "red"
    )
  })
  
  
  # Baca data dari Excel dan olah clustering
  data_clust <- read_excel("data/Data Timbulan Sampah.xlsx")
  
  # Pembersihan dan normalisasi (sesuaikan kolomnya ya)
  data_num <- data_clust %>% 
    select('Timbulan Sampah Tahunan', 'Sisa Makanan', 'Jumlah TPA') %>%
    na.omit() %>%
    scale()
  
  set.seed(123)
  kmeans_result <- kmeans(data_num, centers = 3, nstart = 25)
  
  # Tambah kolom cluster ke data asli (pastikan data asli sesuai barisnya)
  data_clust$cluster <- as.factor(kmeans_result$cluster)
  
  # Baca shapefile Indonesia
  peta <- st_read("Provinsi Indonesia.json")
  names(peta)
  
  
  # Gabungkan data clustering dengan shapefile (sesuaikan nama kolom join)
  cluster_data <- data_clust %>%
    select(Provinsi, cluster) %>%
    rename(PROVINSI = Provinsi)
  
  peta_klaster <- peta %>%
    left_join(cluster_data, by = "PROVINSI")
  
  print(peta_klaster[, c("PROVINSI", "cluster")])
  
  
  # Render leaflet di output$petaCluster
  output$petaCluster <- renderLeaflet({
    pal <- colorFactor(
      palette = c("red", "green", "blue"),
      domain = peta_klaster$cluster
    )
    
    leaflet(peta_klaster) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(cluster),
        color = "white",
        weight = 1,
        popup = ~paste(PROVINSI, "<br>Cluster:", cluster),
        fillOpacity = 0.8
      )
  })
  
  # Ringkasan jumlah per cluster
  # Ringkasan jumlah per cluster
  output$jumlahCluster1 <- renderValueBox({
    valueBox(
      value = tags$div(sum(data_clust$cluster == 1), style = "font-size: 30px; font-weight: bold; color: white;"),
      subtitle = tags$div("Wilayah di Cluster 1", style = "color: white;"),
      icon = icon("layer-group"),
      color = "red"
    )
  })
  
  output$jumlahCluster2 <- renderValueBox({
    valueBox(
      value = tags$div(sum(data_clust$cluster == 2), style = "font-size: 30px; font-weight: bold; color: white;"),
      subtitle = tags$div("Wilayah di Cluster 2", style = "color: white;"),
      icon = icon("layer-group"),
      color = "green"
    )
  })
  
  output$jumlahCluster3 <- renderValueBox({
    valueBox(
      value = tags$div(sum(data_clust$cluster == 3), style = "font-size: 30px; font-weight: bold; color: white;"),
      subtitle = tags$div("Wilayah di Cluster 3", style = "color: white;"),
      icon = icon("layer-group"),
      color = "blue"
    )
  })
  
  # Bar Chart
  output$barCluster <- renderPlot({
    bar_data <- as.data.frame(table(data_clust$cluster))
    
    barplot(bar_data$Freq, 
            names.arg = paste("Cluster", bar_data$Var1),
            col = c("#e74c3c", "#2ecc71", "#3498db"),  # merah, hijau, biru lembut
            main = "Jumlah Wilayah per Cluster",
            xlab = "Cluster",
            ylab = "Jumlah Wilayah",
            border = NA,
            ylim = c(0, max(bar_data$Freq) + 2),
            cex.names = 1.2,
            cex.axis = 1,
            cex.main = 1.4)
  })
  
  # Boxplot
  output$boxplotCluster <- renderPlot({
    boxplot(data_clust$`Timbulan Sampah Tahunan` ~ data_clust$cluster,
            main = "Distribusi Timbulan Sampah per Cluster",
            col = scales::alpha(c("#e74c3c", "#2ecc71", "#3498db"), 0.5),
            xlab = "Cluster",
            ylab = "Timbulan Sampah Tahunan (Ton)",
            border = "gray30",
            cex.axis = 1.1,
            cex.lab = 1.2,
            cex.main = 1.4)
  })
  
  # Radar chart
  output$radarCluster <- renderPlot({
    
    mean_by_cluster <- data_clust %>%
      group_by(cluster) %>%
      summarise(
        Timbulan = mean(`Timbulan Sampah Tahunan`, na.rm = TRUE),
        SisaMakanan = mean(`Sisa Makanan`, na.rm = TRUE),
        TPA = mean(`Jumlah TPA`, na.rm = TRUE)
      )
    
    radar_df <- as.data.frame(mean_by_cluster[,-1])
    rownames(radar_df) <- paste("Cluster", mean_by_cluster$cluster)
    
    max_min <- rbind(apply(radar_df, 2, max), apply(radar_df, 2, min))
    radar_data <- rbind(max_min, radar_df)
    
    radarchart(radar_data,
               axistype = 1,
               pcol = c("#e74c3c", "#2ecc71", "#3498db"),
               pfcol = scales::alpha(c("#e74c3c", "#2ecc71", "#3498db"), 0.3),
               plwd = 2,
               plty = 1,
               cglcol = "gray70",
               cglty = 1,
               axislabcol = "gray30",
               vlcex = 1.1,
               title = "Karakteristik Rata-Rata per Cluster")
    legend("topright", legend = rownames(radar_df),
           col = c("#e74c3c", "#2ecc71", "#3498db"),
           lty = 1, bty = "n", cex = 0.9)
  })
  
  output$interpretasiRadar <- renderText({
    paste(
      "🧭 Interpretasi Radar Chart:",
      "- Cluster 1 memiliki nilai rendah pada semua variabel, cenderung menggambarkan wilayah rural atau berpenduduk rendah",
      "- Cluster 2 memiliki nilai rata-rata tertinggi untuk Timbulan Sampah dan Sisa Makanan, mencerminkan wilayah dengan konsumsi dan pembuangan tinggi.",
      "- Cluster 3 menunjukkan nilai menengah pada semua indikator, menandakan wilayah yang relatif seimbang.",
      "",
      "📌 Insight ini dapat digunakan sebagai dasar perencanaan kebijakan pengelolaan FLW yang lebih efektif.",
      sep = "\n"
    )
  })
  
  
  
  # DataTable
  output$tabelCluster <- renderDataTable({
    DT::datatable(data_clust, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Insight singkat
  output$insightCluster <- renderUI({
    HTML(
      "<div style='white-space: normal; width: 100%; font-size: 16px; line-height: 1.5;'>
      <strong>Insight dan Analisis Clustering:</strong><br><br>
      Dalam upaya mengidentifikasi wilayah dengan potensi besar dalam pengelolaan <em>Food Loss and Waste</em> (FLW), digunakan metode K-Means Clustering.
      Metode ini membagi provinsi di Indonesia ke dalam tiga cluster berdasarkan rata-rata produksi FLW, sisa makanan, dan jumlah Tempat Pembuangan Akhir (TPA).<br><br>
      
      <strong style='color: red;'>Cluster 1</strong> terdiri dari provinsi dengan tingkat FLW dan fasilitas pengelolaan rendah.
      Wilayah dalam cluster ini cenderung rural atau berpenduduk lebih sedikit, sehingga menghasilkan lebih sedikit sampah. Fokus kebijakan di sini bisa pada penguatan sistem pengumpulan dan edukasi pengelolaan sampah.<br><br>
       
      <strong style='color: blue;'>Cluster 2</strong> meliputi provinsi dengan tingkat FLW tinggi dan banyak TPA.
      Ini adalah wilayah urban atau padat penduduk dengan beban pengelolaan limbah yang besar. Perlu pendekatan berbasis teknologi, sistem daur ulang terintegrasi, dan regulasi yang lebih ketat untuk mengurangi timbulan FLW.<br><br>

      <strong style='color: green;'>Cluster 3</strong> mencakup wilayah dengan tingkat FLW dan jumlah TPA sedang.
      Wilayah-wilayah ini relatif seimbang antara produksi sampah dan kapasitas pengelolaan, sehingga cocok untuk peningkatan efisiensi pengelolaan dan inovasi lokal.<br><br>
      
     
      Hasil clustering ini memberikan pemahaman yang lebih dalam tentang karakteristik regional, sehingga dapat digunakan sebagai dasar dalam perumusan kebijakan pengelolaan FLW yang lebih efektif dan tepat sasaran.
    </div>"
    )
  })
  
  output$dataset_selector <- renderUI({
    selectInput("dataset", "Pilih Dataset",
                choices = names(list_datasets()))
  })
  
  # Data PLTSa
  pltsa_data <- tryCatch({
    read_excel("data/LokasiKapasitasPLTSa(MW).xlsx") %>% 
      select(Lokasi = lokasi, `Kapasitas (MW)` = kapasitas)
  }, error = function(e) {
    data.frame(Lokasi = character(), `Kapasitas (MW)` = numeric())
  })
  
  
  # List dataset tersedia
  list_datasets <- reactive({
    list(
      "Data Clustering" = data_clust,
      "Data PLTSA" = pltsa_data,
      "Data Gas Rumah Kaca" = dataGRK
    )
  })
  
  # Selector untuk memilih dataset
  output$dataset_selector <- renderUI({
    selectInput("dataset", "Pilih Dataset",
                choices = names(list_datasets()))
  })
  
  # Dataset yang dipilih user
  datasetInput <- reactive({
    req(input$dataset)
    list_datasets()[[input$dataset]]
  })
  
  # Render UI untuk variabel numerik
  output$var_select <- renderUI({
    data <- datasetInput()
    req(data)
    
    num_vars <- names(data)[sapply(data, is.numeric)]
    
    selectInput("var_normalitas", "Pilih Variabel:",
                choices = num_vars)
  })
  
  # Kategori Sampel
  kategori_sampel <- reactive({
    req(input$var_normalitas)
    data <- datasetInput()
    n <- length(na.omit(data[[input$var_normalitas]]))
    if (n <= 30) return("kecil") else return("besar")
  })
  
  # Kategori Uji
  kategori_uji <- reactive({
    req(input$metode_uji)
    if (input$metode_uji %in% c("shapiro", "lf")) {
      return("kecil")
    } else {
      return("besar")
    }
  })
  
  # Peringatan Validasi
  output$peringatan_validasi <- renderUI({
    req(kategori_sampel(), kategori_uji())
    
    if (kategori_sampel() != kategori_uji()) {
      tipe <- ifelse(kategori_sampel() == "kecil", "sampel kecil", "sampel besar")
      saran <- ifelse(kategori_sampel() == "kecil",
                      "Shapiro-Wilk atau Liliefors",
                      "Kolmogorov-Smirnov, Jarque-Bera, atau Goodness of Fit")
      
      div(
        style = "color: red; font-weight: bold; padding: 10px; border: 1px solid red; border-radius: 5px;",
        paste("⚠️ Peringatan: Data termasuk", tipe, 
              "namun Anda memilih metode uji untuk sampel", kategori_uji(), "."),
        br(),
        paste("Disarankan menggunakan metode uji:", saran)
      )
    }
  })
  
  # Jalankan uji saat tombol ditekan
  hasil_uji <- eventReactive(input$run_test, {
    req(input$var_normalitas, input$metode_uji)
    
    data <- datasetInput()
    req(data)
    
    var_data <- data[[input$var_normalitas]]
    
    if (is.factor(var_data) || is.character(var_data)) {
      var_data <- as.numeric(as.character(var_data))
    }
    
    validate(
      need(is.numeric(var_data), "Variabel yang dipilih harus bertipe numerik."),
      need(!all(is.na(var_data)), "Semua data adalah NA setelah konversi."),
      need(length(var_data) > 2, "Jumlah data terlalu sedikit untuk diuji."),
      need(!anyNA(var_data), "Data mengandung NA. Harap bersihkan dulu.")
    )
    
    # Validasi kecocokan metode uji dan jenis sampel
    if (kategori_sampel() != kategori_uji()) {
      tipe <- ifelse(kategori_sampel() == "kecil", "sampel kecil", "sampel besar")
      saran <- ifelse(kategori_sampel() == "kecil",
                      "Shapiro-Wilk atau Liliefors",
                      "Kolmogorov-Smirnov, Jarque-Bera, atau Goodness of Fit")
      
      validate(
        need(FALSE, paste0("Metode uji tidak sesuai. Data adalah ", tipe,
                           ", disarankan menggunakan metode: ", saran, "."))
      )
    }
    
    hasil <- switch(input$metode_uji,
                    "shapiro" = shapiro.test(var_data),
                    "ks" = ks.test(var_data, "pnorm", mean = mean(var_data), sd = sd(var_data)),
                    "lf" = nortest::lillie.test(var_data),
                    "jb" = tseries::jarque.bera.test(var_data),
                    "gf" = nortest::ad.test(var_data),
                    {
                      validate(need(FALSE, "Metode uji tidak dikenali."))
                      NULL
                    }
    )
    
    return(hasil)
  })

  # A. Hipotesis
  output$hipotesis <- renderUI({
    HTML(paste0(
      "<div style='background-color:#34495e; color:white; padding:15px; border-left:5px solid #00BCD4; border-radius:5px;'>",
      "<p><b>H₀:</b> Data <span style='color:#2ecc71;'>berdistribusi normal</span></p>",
      "<p><b>H₁:</b> Data <span style='color:#e74c3c;'>tidak berdistribusi normal</span></p>",
      "</div>"
    ))
  })
  
  # B. Tingkat Signifikansi
  output$sig_level <- renderText({
    paste0("Tingkat signifikansi: α = ", input$alpha)
  })
  
  # C. Statistik Uji
  output$stat_uji <- renderUI({
    hasil <- hasil_uji()
    req(hasil)
    
    HTML(paste0(
      "<div style='background-color:#f39c12; color:white; padding:15px; border-left:5px solid #e67e22; border-radius:5px;'>",
      "<p><b>Statistik Uji:</b> ", round(hasil$statistic, 4), "</p>",
      "<p><b>p-value:</b> ", signif(hasil$p.value, 4), "</p>",
      "</div>"
    ))
  })
  
  # D. Keputusan
  output$keputusan <- renderText({
    hasil <- hasil_uji()
    req(hasil)
    
    if (hasil$p.value < input$alpha) {
      "Tolak H₀"
    } else {
      "Gagal Tolak H₀"
    }
  })
  
  # E. Kesimpulan
  output$kesimpulan <- renderText({
    hasil <- hasil_uji()
    req(hasil)
    
    if (hasil$p.value < input$alpha) {
      paste0("Dengan tingkat signifikansi sebesar ", input$alpha, ", data tidak berdistribusi normal.")
    } else {
      paste0("Dengan tingkat signifikansi sebesar ", input$alpha, ", data berdistribusi normal.")
    }
  })
  
  # F. Visualisasi
  output$hist_plot_normal <- renderPlot({
    req(input$var_normalitas)
    data <- datasetInput()
    req(data)
    
    var_data <- data[[input$var_normalitas]]
    var_data <- na.omit(var_data)
    
    # Pastikan numerik
    if (is.factor(var_data) || is.character(var_data)) {
      var_data <- as.numeric(as.character(var_data))
    }
    validate(need(is.numeric(var_data), "Variabel harus numerik"))
    
    # Plot histogram dengan kurva normal
    hist(var_data,
         probability = TRUE,
         main = paste("Histogram dan Kurva Normal:", input$var_normalitas),
         xlab = input$var_normalitas,
         col = "lightblue",
         border = "white")
    
    # Tambahkan kurva normal
    curve(dnorm(x, mean = mean(var_data), sd = sd(var_data)),
          col = "red", lwd = 2, add = TRUE)
  })
  
  # MENU UNTUK UJI KORELASI
  
  # Pilih dataset
  output$dataset_selector_korelasi <- renderUI({
    selectInput("dataset_korelasi", "Pilih Dataset:",
                choices = names(list_datasets()))
  })
  
  # Reactive: Ambil dataset yang dipilih
  datasetInput_korelasi <- reactive({
    req(input$dataset_korelasi)
    list_datasets()[[input$dataset_korelasi]]
  })
  
  # Pilih variabel X
  output$var_x_select <- renderUI({
    data <- datasetInput_korelasi()
    req(data)
    num_vars <- names(Filter(is.numeric, data))
    selectInput("var_x", "Pilih Variabel X:", choices = num_vars)
  })
  
  # Pilih variabel Y
  output$var_y_select <- renderUI({
    data <- datasetInput_korelasi()
    req(data)
    num_vars <- names(Filter(is.numeric, data))
    selectInput("var_y", "Pilih Variabel Y:", choices = num_vars)
  })
  
  # Validasi input X ≠ Y
  output$peringatan_validasi_korelasi <- renderUI({
    req(input$var_x, input$var_y)
    if (input$var_x == input$var_y) {
      div(style = "color:red;", "Variabel X dan Y tidak boleh sama.")
    }
  })
  
  # Reactive: Ambil data 2 kolom numeric yang dipilih
  data_korelasi <- reactive({
    req(datasetInput_korelasi(), input$var_x, input$var_y)
    data <- datasetInput_korelasi()
    df <- data[, c(input$var_x, input$var_y)]
    
    # Pastikan kedua kolom numeric
    df[[input$var_x]] <- as.numeric(df[[input$var_x]])
    df[[input$var_y]] <- as.numeric(df[[input$var_y]])
    
    na.omit(df)
  })
  
  # Uji normalitas untuk variabel X
  hasil_normal_x <- reactive({
    req(input$var_x)
    data_x <- datasetInput_korelasi()[[input$var_x]]
    n_x <- length(data_x)
    
    if (n_x < 30) {
      shapiro.test(data_x)
    } else {
      ks.test(data_x, "pnorm", mean = mean(data_x, na.rm = TRUE), sd = sd(data_x, na.rm = TRUE))
    }
  })
  
  # Uji normalitas untuk variabel Y
  hasil_normal_y <- reactive({
    req(input$var_y)
    data_y <- datasetInput_korelasi()[[input$var_y]]
    n_y <- length(data_y)
    
    if (n_y < 30) {
      shapiro.test(data_y)
    } else {
      ks.test(data_y, "pnorm", mean = mean(data_y, na.rm = TRUE), sd = sd(data_y, na.rm = TRUE))
    }
  })
  
  # Validasi metode korelasi
  output$peringatan_validasi_korelasi <- renderUI({
    req(input$metode_korelasi, hasil_normal_x(), hasil_normal_y(), input$alpha_korelasi)
    
    p_x <- hasil_normal_x()$p.value
    p_y <- hasil_normal_y()$p.value
    
    if (input$metode_korelasi == "pearson") {
      if (!is.null(p_x) && !is.null(p_y) &&
          (p_x < input$alpha_korelasi || p_y < input$alpha_korelasi)) {
        return(HTML("<div style='color:red; margin-bottom:5px;'><b>⚠️ Peringatan:</b> Salah satu atau kedua variabel tidak berdistribusi normal. Uji <b>Pearson</b> mungkin tidak tepat. Pertimbangkan <b>Spearman</b> atau <b>Kendall</b>.</div>"))
      }
    } else {
      if (!is.null(p_x) && !is.null(p_y) &&
          (p_x >= input$alpha_korelasi && p_y >= input$alpha_korelasi)) {
        return(HTML("<div style='color:orange; margin-bottom:5px;'><b>ℹ️ Catatan:</b> Kedua variabel berdistribusi normal. Anda dapat menggunakan <b>Pearson</b> untuk hasil yang lebih presisi.</div>"))
      }
    }
    
    return(NULL)  # Tidak ada peringatan
  })
  
  
  # Jalankan uji korelasi
  hasil_korelasi <- eventReactive(input$run_korelasi, {
    req(input$var_x, input$var_y, input$metode_korelasi)
    
    x <- data_korelasi()[[input$var_x]]
    y <- data_korelasi()[[input$var_y]]
    
    # Cek validasi jika Pearson
    if (input$metode_korelasi == "pearson") {
      p_x <- hasil_normal_x()$p.value
      p_y <- hasil_normal_y()$p.value
      
      if (p_x < input$alpha_korelasi || p_y < input$alpha_korelasi) {
        # Return NULL jika syarat tidak terpenuhi
        return(NULL)
      }
    }
    
    # Jalankan korelasi kalau semua lolos
    cor.test(x, y, method = input$metode_korelasi)
  })
  
  # Hipotesis
  output$hipotesis_korelasi <- renderUI({
    req(input$var_x, input$var_y)
    HTML(paste0(
      "<div style='background-color:#2c3e50; padding:15px; border-left:5px solid #2980b9;
                 border-radius:8px; font-size:14px; color:white;'>",
      "<p><b>H₀:</b> <span style='color:#2ecc71;'>Tidak ada korelasi</span> antara <b>", input$var_x, "</b> dan <b>", input$var_y, "</b></p>",
      "<p><b>H₁:</b> <span style='color:#e74c3c;'>Terdapat korelasi</span> antara <b>", input$var_x, "</b> dan <b>", input$var_y, "</b></p>",
      "</div>"
    ))
  })
  
  # Tingkat signifikansi
  output$sig_level_korelasi <- renderText({
    req(input$alpha_korelasi)
    paste("Tingkat signifikansi: α =", input$alpha_korelasi)
  })
  
  # Statistik uji
  output$stat_uji_korelasi <- renderUI({
    hasil <- hasil_korelasi()
    req(hasil)
    
    HTML(paste0(
      "<div style='background-color:#fcf3cf; padding:12px; border-left:5px solid #f1c40f;
                 border-radius:6px; font-size:14px; color:#4d4d4d;'>",
      "<p><b>Nilai r:</b> ", round(hasil$estimate, 4), "</p>",
      "<p><b>Statistik Uji (t):</b> ", round(hasil$statistic, 4), "</p>",
      "<p><b>p-value:</b> ", signif(hasil$p.value, 4), "</p>",
      "</div>"
    ))
  })
  
  # Keputusan
  output$keputusan_korelasi <- renderText({
    hasil <- hasil_korelasi()
    req(hasil)
    if (hasil$p.value < input$alpha_korelasi) {
      "Tolak H₀"
    } else {
      "Gagal Tolak H₀"
    }
  })
  
  # Kesimpulan
  output$kesimpulan_korelasi <- renderText({
    hasil <- hasil_korelasi()
    req(hasil)
    if (hasil$p.value < input$alpha_korelasi) {
      paste("Terdapat hubungan korelasi yang signifikan antara", input$var_x, "dan", input$var_y)
    } else {
      paste("Tidak terdapat hubungan korelasi yang signifikan antara", input$var_x, "dan", input$var_y)
    }
  })
  
  # Scatter plot visualisasi
  output$scatter_plot_korelasi <- renderPlot({
    df <- data_korelasi()
    req(df)
    x <- df[[input$var_x]]
    y <- df[[input$var_y]]
    
    plot(x, y,
         main = paste("Scatter Plot", input$var_x, "vs", input$var_y),
         xlab = input$var_x, ylab = input$var_y,
         pch = 19, col = "steelblue")
    
    abline(lm(y ~ x), col = "red", lwd = 2)
  })
  
  # Data dan Model
  dataGRK <- data.frame(
    Tahun = 2000:2019,
    Emisi = c(50.833, 52.277, 53.681, 56.054, 58.02, 60.413, 62.273, 66.748, 
              71.488, 76.135, 82.784, 90.894, 92.014, 97.52, 100.902, 109.642, 
              115.482, 125.53, 140.403, 139.971)
  )
  
  modelGRK <- lm(Emisi ~ Tahun, data = dataGRK)
  tahun_proyeksi <- 2020:2050
  proyeksi <- predict(modelGRK, newdata = data.frame(Tahun = tahun_proyeksi), interval = "prediction")
  
  hasil_proyeksi <- data.frame(
    Tahun = tahun_proyeksi, 
    Proyeksi_Emisi = round(proyeksi[,1], 2),
    Lower_CI = round(proyeksi[,2], 2),
    Upper_CI = round(proyeksi[,3], 2)
  )
  
  r_squared <- summary(modelGRK)$r.squared
  adj_r_squared <- summary(modelGRK)$adj.r.squared
  rmse <- sqrt(mean(modelGRK$residuals^2))
  
  # Output nilai box
  output$proyeksi_2025 <- renderValueBox({
    valueBox(
      value = paste(hasil_proyeksi$Proyeksi_Emisi[hasil_proyeksi$Tahun == 2025], "Mton CO₂e"),
      subtitle = "🌍 Proyeksi Emisi 2025",
      icon = icon("chart-line"),
      color = "red",
      width = 4
    )
  })
  
  output$proyeksi_2045 <- renderValueBox({
    valueBox(
      value = paste(hasil_proyeksi$Proyeksi_Emisi[hasil_proyeksi$Tahun == 2045], "Mton CO₂e"),
      subtitle = "🚀 Proyeksi Emisi 2045",
      icon = icon("arrow-up"),
      color = "orange",
      width = 4
    )
  })
  
  output$peningkatan_rate <- renderValueBox({
    rate <- ((tail(dataGRK$Emisi, 1) / dataGRK$Emisi[1])^(1 / (length(dataGRK$Tahun) - 1)) - 1) * 100
    valueBox(
      value = paste0(round(rate, 2), " %"),
      subtitle = "📈 Rata-rata Kenaikan Emisi/Tahun",
      icon = icon("percentage"),
      color = "yellow",
      width = 4
    )
  })
  
  output$hasil_proyeksi_custom <- renderText({
    t <- input$tahun_custom
    if (t >= 2020 && t <= 2050) {
      paste("Proyeksi emisi GRK dari FLW pada tahun", t, "adalah", 
            round(predict(modelGRK, newdata = data.frame(Tahun = t)), 2), "Mton CO2eq")
    } else {
      "Silakan masukkan tahun antara 2020-2050"
    }
  })
  
  output$plotEmisiInteraktif <- renderPlotly({
    data_historis <- data.frame(Tahun = dataGRK$Tahun, Emisi = dataGRK$Emisi, Tipe = "Historis")
    data_proyeksi <- data.frame(Tahun = hasil_proyeksi$Tahun, Emisi = hasil_proyeksi$Proyeksi_Emisi, Tipe = "Proyeksi")
    
    p <- plot_ly() %>%
      add_trace(data = data_historis, x = ~Tahun, y = ~Emisi, type = 'scatter', mode = 'lines+markers',
                name = 'Data Historis', line = list(color = '#2E86AB')) %>%
      add_trace(data = data_proyeksi, x = ~Tahun, y = ~Emisi, type = 'scatter', mode = 'lines+markers',
                name = 'Proyeksi', line = list(color = '#A23B72', dash = 'dash'))
    
    if (input$show_ci == TRUE) {
      p <- p %>%
        add_ribbons(data = hasil_proyeksi, x = ~Tahun, ymin = ~Lower_CI, ymax = ~Upper_CI,
                    fillcolor = 'rgba(162, 59, 114, 0.2)', line = list(color = 'transparent'),
                    name = 'Confidence Interval (95%)')
    }
    
    p %>% layout(title = "Proyeksi Emisi GRK dari Food Loss & Waste",
                 xaxis = list(title = "Tahun"),
                 yaxis = list(title = "Emisi (Mton CO2eq)"),
                 hovermode = 'x unified')
  })
  
  output$plotEmisiStatis <- renderPlot({
    ggplot() +
      geom_line(data = dataGRK, aes(x = Tahun, y = Emisi, color = "Historis")) +
      geom_point(data = dataGRK, aes(x = Tahun, y = Emisi, color = "Historis")) +
      geom_line(data = hasil_proyeksi, aes(x = Tahun, y = Proyeksi_Emisi, color = "Proyeksi"), linetype = "dashed") +
      geom_point(data = hasil_proyeksi, aes(x = Tahun, y = Proyeksi_Emisi, color = "Proyeksi")) +
      scale_color_manual(values = c("Historis" = "#2E86AB", "Proyeksi" = "#A23B72")) +
      labs(title = "Proyeksi Emisi GRK dari Food Loss & Waste", x = "Tahun", y = "Emisi (Mton CO2eq)", color = "Tipe Data") +
      theme_minimal() + theme(legend.position = "bottom")
  })
  
  output$tabelProyeksi <- DT::renderDataTable({
    DT::datatable(hasil_proyeksi, options = list(pageLength = 10, scrollY = "250px"))
  })
  
  output$analisis_trend <- renderText({
    paste("Emisi GRK dari FLW mengalami peningkatan rata-rata", 
          round(coef(modelGRK)[2], 2), "Mton CO2eq per tahun dalam periode 2000-2019.")
  })
  
  output$analisis_dampak <- renderText({
    inc <- ((hasil_proyeksi$Proyeksi_Emisi[hasil_proyeksi$Tahun == 2045] - tail(dataGRK$Emisi, 1)) / 
              tail(dataGRK$Emisi, 1)) * 100
    paste("Tanpa intervensi, emisi diproyeksikan meningkat", round(inc, 1), "% dari 2019 hingga 2045.")
  })
  
  output$rekomendasi <- renderUI({
    tags$ul(
      tags$li("perubahan perilaku"),
      tags$li("Pembenahan Penunjang Sistem Pangan"),
      tags$li("Penguatan regulasi dan optimalisasi pendanaan"),
      tags$li("Pemanfaatan FLW"),
      tags$li("Pengembangan Kajian dan Pendataan FLW")
    )
  })
  
  output$plotSkenario <- renderPlotly({
    skenario_10 <- hasil_proyeksi$Proyeksi_Emisi * 0.9
    skenario_25 <- hasil_proyeksi$Proyeksi_Emisi * 0.75
    skenario_50 <- hasil_proyeksi$Proyeksi_Emisi * 0.5
    
    plot_ly() %>%
      add_trace(x = hasil_proyeksi$Tahun, y = hasil_proyeksi$Proyeksi_Emisi, type = 'scatter', mode = 'lines',
                name = 'Business as Usual', line = list(color = '#FF6B6B')) %>%
      add_trace(x = hasil_proyeksi$Tahun, y = skenario_10, type = 'scatter', mode = 'lines',
                name = 'Mitigasi 10%', line = list(color = '#4ECDC4')) %>%
      add_trace(x = hasil_proyeksi$Tahun, y = skenario_25, type = 'scatter', mode = 'lines',
                name = 'Mitigasi 25%', line = list(color = '#45B7D1')) %>%
      add_trace(x = hasil_proyeksi$Tahun, y = skenario_50, type = 'scatter', mode = 'lines',
                name = 'Mitigasi 50%', line = list(color = '#96CEB4')) %>%
      layout(title = "Perbandingan Skenario Mitigasi Emisi GRK",
             xaxis = list(title = "Tahun"),
             yaxis = list(title = "Emisi (Mton CO2eq)"),
             hovermode = 'x unified')
  })
  
  output$statistik_model_ui <- renderUI({
    tags$div(
      h4("Kualitas Model:"),
      tags$ul(
        tags$li(paste("R-squared:", round(r_squared, 4))),
        tags$li(paste("Adj. R-squared:", round(adj_r_squared, 4))),
        tags$li(paste("RMSE:", round(rmse, 2), "Mton CO2eq"))
      ),
      br(),
      h4("Interpretasi:"),
      tags$div(
        style = "font-size: 12px;",
        if (r_squared > 0.8) {
          "Model menunjukkan fit yang baik dengan data historis."
        } else if (r_squared > 0.6) {
          "Model menunjukkan fit yang cukup baik."
        } else {
          "Model menunjukkan fit yang lemah. Pertimbangkan model yang lebih kompleks."
        }
      ),
      br(),
      h4("Persamaan Regresi:"),
      tags$code(paste("Emisi =", round(coef(modelGRK)[1], 2), "+", round(coef(modelGRK)[2], 2), "× Tahun")),
      br(), br(),
      h4("Interpretasi Parameter:"),
      tags$div(
        style = "font-size: 11px;",
        tags$p(paste("b0 (Intercept =", round(coef(modelGRK)[1], 2), 
                     "): Nilai teoritis emisi pada tahun 0. Tidak memiliki interpretasi praktis.")),
        tags$p(paste("b1 (Slope =", round(coef(modelGRK)[2], 2), 
                     "): Setiap penambahan 1 tahun, emisi GRK meningkat rata-rata", 
                     round(coef(modelGRK)[2], 2), "Mton CO2eq."))
      )
    )
  })
  
  # Menu Konversi FLW
  konversi_data <- eventReactive(input$hitung_konversi, {
    flw_ribu_ton <- input$input_flw
    flw_ton <- flw_ribu_ton * 1000
    energi_mwh <- (flw_ton / 750) * 474
    
    list(
      flw_input = flw_ribu_ton,
      flw_ton = flw_ton,
      energi_mwh = energi_mwh
    )
  })
  
  output$output_energi_number <- renderText({
    if (!is.null(input$hitung_konversi) && input$hitung_konversi > 0) {
      data <- konversi_data()
      format(round(data$energi_mwh, 2), big.mark = ",", decimal.mark = ".")
    } else {
      "0"
    }
  })
  
  output$output_energi_description <- renderText({
    if (!is.null(input$hitung_konversi) && input$hitung_konversi > 0) {
      data <- konversi_data()
      paste0("energi listrik dapat dihasilkan dari ", 
             format(data$flw_input, big.mark = ","), 
             " ribu ton FLW.")
    } else {
      "Masukkan nilai FLW dan klik tombol hitung untuk melihat hasil konversi."
    }
  })
  
  # Menu Data PLTSA
  # Fungsi untuk import data
  import_data1 <- function() {
    # Data PLTSa
    pltsa_data <- tryCatch({
      read_excel("data/LokasiKapasitasPLTSa(MW).xlsx") %>% 
        select(Lokasi = lokasi, `Kapasitas (MW)` = kapasitas)
    }, error = function(e) {
      data.frame(Lokasi = character(), `Kapasitas (MW)` = numeric())
    })
    
    # Data TPA
    tpa_data <- tryCatch({
      data_clust %>% 
        select(Provinsi, `Jumlah TPA`)
    }, error = function(e) {
      data.frame(Provinsi = character(), `Jumlah TPA` = numeric())
    })
    
    return(list(pltsa = pltsa_data, tpa = tpa_data))
  }
  
  data1 <- reactive({
    import_data1()
  })
  
  output$total_pltsa <- renderValueBox({
    total <- sum(data1()$pltsa$`Kapasitas (MW)`, na.rm = TRUE)
    valueBox(value = paste0(total, " MW"), subtitle = "Total Kapasitas PLTSa", icon = icon("bolt"), color = "red")
  })
  
  output$total_tpa <- renderValueBox({
    total <- sum(data1()$tpa$`Jumlah TPA`, na.rm = TRUE)
    valueBox(value = total, subtitle = "Total Jumlah TPA", icon = icon("trash"), color = "orange")
  })
  
  output$prov_terbanyak_tpa <- renderValueBox({
    tpa_df <- data1()$tpa
    if (nrow(tpa_df) > 0) {
      top_row <- tpa_df[which.max(tpa_df$`Jumlah TPA`), ]
      valueBox(value = top_row$`Jumlah TPA`, subtitle = paste("TPA Terbanyak:", top_row$Provinsi), icon = icon("map-marker-alt"), color = "blue")
    } else {
      valueBox(value = "-", subtitle = "TPA Terbanyak: Tidak Ada", icon = icon("map-marker-alt"), color = "blue")
    }
  })
  
  output$pltsa_table <- renderDT({
    datatable(
      data1()$pltsa,
      options = list(pageLength = 10, searching = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>% formatRound(columns = 'Kapasitas (MW)', digits = 1)
  })
  
  output$tpa_table <- renderDT({
    datatable(
      data1()$tpa,
      options = list(pageLength = 10, searching = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  # Menu Proyeksi Listrik
  # Fungsi untuk import data
  import_data <- function() {
    tryCatch({
      read_excel("data/Konsumsi Listrik Nasional.xlsx") %>% 
        select(Tahun, `Konsumsi Listrik` = `Konsumsi Tenaga Listrik`)
    }, error = function(e) {
      data.frame(Tahun = integer(), `Konsumsi Listrik` = numeric())
    })
  }
  
  data <- reactive({
    import_data()
  })
  
  # Membuat model regresi
  model <- reactive({
    lm(`Konsumsi Listrik` ~ Tahun, data = data())
  })
  
  # Value Box
  output$total_konsumsi <- renderValueBox({
    total <- sum(data()$`Konsumsi Listrik`, na.rm = TRUE)
    valueBox(
      value = paste0(format(round(total, 2), big.mark = ".", decimal.mark = ","), " GWh"),
      subtitle = "Total Konsumsi Listrik",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  output$rata_konsumsi <- renderValueBox({
    rata <- mean(data()$`Konsumsi Listrik`, na.rm = TRUE)
    valueBox(
      value = paste0(format(round(rata, 2), big.mark = ".", decimal.mark = ","), " GWh"),
      subtitle = "Rata-rata Konsumsi Listrik",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$prediksi_tahun <- renderValueBox({
    tahun <- input$tahun_input
    prediksi <- predict(model(), newdata = data.frame(Tahun = tahun))
    valueBox(
      value = paste0(format(round(prediksi, 2), big.mark = ".", decimal.mark = ","), " GWh"),
      subtitle = paste("Prediksi Tahun", tahun),
      icon = icon("arrow-up"),
      color = "purple"
    )
  })
  
  # Tabel Data Konsumsi Listrik
  output$tabel_konsumsi <- renderDT({
    datatable(
      data(),
      options = list(
        pageLength = 10,
        searching = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>% 
      formatRound("Konsumsi Listrik", digits = 2)
  })
  
  # Plot Regresi Linier
  output$plot_regresi <- renderPlot({
    data_konsumsi <- data()  
    model_regresi <- model()
    
    tahun_max <- input$tahun_input
    
    tahun_range <- seq(min(data_konsumsi$Tahun), tahun_max, by = 1)
    prediksi_data <- data.frame(
      Tahun = tahun_range,
      Prediksi = predict(model_regresi, newdata = data.frame(Tahun = tahun_range))
    )
    
    ggplot(data_konsumsi, aes(x = Tahun, y = `Konsumsi Listrik`)) +
      geom_point(color = "#3498db", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "#e74c3c", fill = "#ecf0f1") +
      geom_line(data = prediksi_data, aes(x = Tahun, y = Prediksi), 
                color = "#f39c12", linetype = "dashed", size = 1) +
      geom_point(data = prediksi_data[prediksi_data$Tahun > max(data_konsumsi$Tahun),], 
                 aes(x = Tahun, y = Prediksi), color = "#f39c12", size = 3) +
      labs(title = "Regresi Linier Konsumsi Listrik Nasional",
           x = "Tahun", 
           y = "Konsumsi Listrik (GWh)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
  })
  
  # Model Summary
  output$hasil_summary <- renderUI({
    summary_model <- summary(model())
    coef <- summary_model$coefficients
    
    b0 <- round(coef[1,1], 2)
    b1 <- round(coef[2,1], 2)
    
    rse <- round(summary_model$sigma, 4)
    r_squared <- round(summary_model$r.squared, 4)
    adj_r_squared <- round(summary_model$adj.r.squared, 4)
    
    withMathJax(HTML(paste0(
      "<h4><strong>Persamaan Regresi Linier:</strong></h4>",
      "<div style='text-align: center; font-size: 16px; margin: 15px 0;'>",
      "$$\\text{Konsumsi Listrik} = ", b0, " + ", b1, " \\times \\text{Tahun}$$",
      "</div>",
      
      "<h4><strong>Summary Model:</strong></h4>",
      "<ul style='list-style-type: none; padding-left: 0;'>",
      "<li>\\(\\text{Residual Standard Error} = ", rse, " \\)</li>",
      "<li>\\(\\text{Multiple R-squared} = ", r_squared, " \\)</li>",
      "<li>\\(\\text{Adjusted R-squared} = ", adj_r_squared, " \\)</li>",
      "</ul>",
      
      "<h4><strong>Interpretasi:</strong></h4>",
      "<p style='text-align: justify; font-size: 16px'>",
      "Koefisien regresi pada persamaan di atas dapat diinterpretasikan sebagai berikut:",
      "</p>",
      "<ul style='text-align: justify; font-size: 16px'>",
      "<li>Perkiraan konsumsi listrik saat tahun sama dengan 0 adalah ", b0, " GWh</li>",
      "<li>Jika tahun naik 1 satuan, maka konsumsi listrik akan naik sebesar ", b1, " GWh.</li>",
      "</ul>"
    )))
  })
  
  # Tabel prediksi
  output$tabel_prediksi <- renderDT({
    tahun_prediksi <- 2024:input$tahun_input
    prediksi <- predict(model(), newdata = data.frame(Tahun = tahun_prediksi))
    
    pred_data <- data.frame(
      Tahun = tahun_prediksi,
      `Prediksi Konsumsi Listrik` = prediksi,
      check.names = FALSE
    )
    
    datatable(
      pred_data,
      options = list(
        pageLength = 10, 
        searching = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>% 
      formatRound("Prediksi Konsumsi Listrik", digits = 2)
  })
  
  # Uji asumsi klasik
  output$uji_unbiased <- renderUI({
    res <- residuals(model())
    mean_res <- mean(res)
    p_value <- t.test(res)$p.value
    
    withMathJax(HTML(paste0(
      "<p><strong>Uji Error Unbiased</strong></p>",
      "<p>\\(\\text{Mean Residual} = ", round(mean_res, 4), "\\)</p>",
      "<p>\\(p\\text{-value} = ", format.pval(p_value, digits = 3), "\\)</p>",
      ifelse(p_value > 0.05,
             "<span style='background-color: #5cb85c; color: white; padding: 4px 8px; border-radius: 4px;'>Tidak ada bias (error unbiased)</span>",
             "<span style='background-color: #d9534f; color: white; padding: 4px 8px; border-radius: 4px;'>Terdapat bias (error biased)</span>")
    )))
  })
  
  output$uji_homoskedastisitas <- renderUI({
    test_result <- bptest(model())
    
    withMathJax(HTML(paste0(
      "<p><strong>Uji Breusch-Pagan</strong></p>",
      "<p>\\(\\chi^2 = ", round(test_result$statistic, 3), "\\)</p>",
      "<p>\\(p\\text{-value} = ", format.pval(test_result$p.value, digits = 3), "\\)</p>",
      ifelse(test_result$p.value > 0.05,
             "<span style='background-color: #5cb85c; color: white; padding: 4px 8px; border-radius: 4px;'>Homoskedastisitas terpenuhi</span>",
             "<span style='background-color: #d9534f; color: white; padding: 4px 8px; border-radius: 4px;'>Heteroskedastisitas terdeteksi</span>")
    )))
  })
  
  output$uji_autokorelasi <- renderUI({
    test_result <- dwtest(model())
    
    withMathJax(HTML(paste0(
      "<p><strong>Uji Durbin-Watson</strong></p>",
      "<p>\\(DW = ", round(test_result$statistic, 3), "\\)</p>",
      "<p>\\(p\\text{-value} = ", format.pval(test_result$p.value, digits = 3), "\\)</p>",
      ifelse(test_result$p.value > 0.05,
             "<span style='background-color: #5cb85c; color: white; padding: 4px 8px; border-radius: 4px;'>Tidak ada autokorelasi</span>",
             "<span style='background-color: #d9534f; color: white; padding: 4px 8px; border-radius: 4px;'>Terdapat autokorelasi</span>")
    )))
  })
  
  output$uji_normalitas <- renderUI({
    test_result <- shapiro.test(residuals(model()))
    
    withMathJax(HTML(paste0(
      "<p><strong>Uji Shapiro-Wilk</strong></p>",
      "<p>\\(W = ", round(test_result$statistic, 3), "\\)</p>",
      "<p>\\(p\\text{-value} = ", format.pval(test_result$p.value, digits = 3), "\\)</p>",
      ifelse(test_result$p.value > 0.05,
             "<span style='background-color: #5cb85c; color: white; padding: 4px 8px; border-radius: 4px;'>Residual berdistribusi normal</span>",
             "<span style='background-color: #d9534f; color: white; padding: 4px 8px; border-radius: 4px;'>Residual tidak berdistribusi normal</span>")
    )))
  })
  
  # Meta Data
  
  # Fungsi untuk mengambil dataset berdasarkan pilihan
  get_dataset_by_choice <- function(choice) {
    switch(choice,
           "clustering" = data_clust,
           "pltsa" = pltsa_data,
           "listrik" = {
             tryCatch({
               read_excel("data/Konsumsi Listrik Nasional.xlsx") %>% 
                 select(Tahun, `Konsumsi Listrik` = `Konsumsi Tenaga Listrik`)
             }, error = function(e) {
               data.frame(Tahun = integer(), `Konsumsi Listrik` = numeric())
             })
           },
           "emisi" = dataGRK
    )
  }
  
  # Preview data reactive
  preview_data <- reactive({
    req(input$dataset_preview)
    dataset <- get_dataset_by_choice(input$dataset_preview)
    return(dataset)
  })
  
  # Render preview table dengan styling yang diperbaiki (TANPA DUPLIKASI PAGINATION)
  output$preview_table <- DT::renderDataTable({
    data <- preview_data()
    
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = FALSE,  # Ubah ke FALSE agar tidak ada horizontal scroll
        autoWidth = FALSE,  # Ubah ke FALSE untuk kontrol width yang lebih baik
        searching = TRUE,
        lengthChange = TRUE,
        info = TRUE,
        ordering = TRUE,
        fixedHeader = FALSE,  # Ubah ke FALSE agar header tidak terpisah
        responsive = TRUE,  # Ubah ke TRUE untuk responsiveness
        columnDefs = list(
          list(
            targets = "_all",
            className = "dt-center"
          )
        ),
        # Sederhanakan language options
        language = list(
          search = "🔍 Cari:",
          lengthMenu = "Tampilkan _MENU_ data per halaman",
          info = "Menampilkan _START_ sampai _END_ dari _TOTAL_ data",
          infoEmpty = "Tidak ada data",
          infoFiltered = "(difilter dari _MAX_ total data)",
          # SEDERHANAKAN pagination tanpa emoji berlebihan
          paginate = list(
            first = "First",
            last = "Last", 
            previous = "Previous",
            "next" = "Next"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover compact',
      extensions = 'Responsive'  # Tambahkan extension Responsive
    ) %>%
      # Format kolom numerik jika ada
      DT::formatRound(columns = which(sapply(data, is.numeric)), digits = 2) %>%
      # Styling untuk header
      DT::formatStyle(
        columns = 1:ncol(data),
        backgroundColor = 'white',
        color = '#2c3e50'
      )
  }, server = TRUE)
  
  # Text output untuk statistik (menggunakan renderText untuk styling yang lebih baik)
  output$dataset_rows_text <- renderText({
    data <- preview_data()
    paste0('<span style="color: white; font-weight: bold; font-size: 24px;">', 
           format(nrow(data), big.mark = ","), 
           '</span>')
  })
  
  output$dataset_cols_text <- renderText({
    data <- preview_data()
    paste0('<span style="color: white; font-weight: bold; font-size: 24px;">', 
           ncol(data), 
           '</span>')
  })
  
  output$dataset_numeric_text <- renderText({
    data <- preview_data()
    paste0('<span style="color: white; font-weight: bold; font-size: 24px;">', 
           sum(sapply(data, is.numeric)), 
           '</span>')
  })
  
  output$dataset_missing_text <- renderText({
    data <- preview_data()
    missing_count <- sum(is.na(data))
    if(missing_count == 0) {
      paste0('<span style="color: white; font-weight: bold; font-size: 24px;">✅ 0</span>')
    } else {
      paste0('<span style="color: white; font-weight: bold; font-size: 24px;">⚠️ ', 
             format(missing_count, big.mark = ","), 
             '</span>')
    }
  })
  
  # Download handler utama (dengan pilihan format)
  output$download_selected <- downloadHandler(
    filename = function() {
      dataset_name <- switch(input$dataset_preview,
                             "clustering" = "data_clustering_flw",
                             "pltsa" = "data_pltsa_indonesia", 
                             "listrik" = "data_konsumsi_listrik_nasional",
                             "emisi" = "data_emisi_grk_indonesia"
      )
      
      extension <- if(input$download_format == "xlsx") ".xlsx" else ".csv"
      paste(dataset_name, "_", Sys.Date(), extension, sep = "")
    },
    content = function(file) {
      data <- get_dataset_by_choice(input$dataset_preview)
      
      if(input$download_format == "xlsx") {
        # Menggunakan writexl untuk export Excel
        if(require(writexl, quietly = TRUE)) {
          writexl::write_xlsx(data, file)
        } else {
          # Fallback ke CSV jika library tidak tersedia
          write.csv(data, file, row.names = FALSE)
          showNotification("Library writexl tidak tersedia, file disimpan dalam format CSV", 
                           type = "warning", duration = 3)
        }
      } else {
        write.csv(data, file, row.names = FALSE)
      }
      
      # Tampilkan notifikasi sukses
      showNotification(
        paste("Dataset berhasil didownload:", basename(file)), 
        type = "success", 
        duration = 3
      )
    }
  )
  
  # Quick download handlers dengan pesan sukses
  output$quick_download_clustering <- downloadHandler(
    filename = function() {
      paste("data_clustering_flw_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_clust, file, row.names = FALSE)
      showNotification("Data Clustering berhasil didownload!", type = "success", duration = 3)
    }
  )
  
  output$quick_download_pltsa <- downloadHandler(
    filename = function() {
      paste("data_pltsa_indonesia_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pltsa_data, file, row.names = FALSE)
      showNotification("Data PLTSa berhasil didownload!", type = "success", duration = 3)
    }
  )
  
  output$quick_download_listrik <- downloadHandler(
    filename = function() {
      paste("data_konsumsi_listrik_nasional_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_listrik <- tryCatch({
        read_excel("data/Konsumsi Listrik Nasional.xlsx") %>% 
          select(Tahun, `Konsumsi Listrik` = `Konsumsi Tenaga Listrik`)
      }, error = function(e) {
        data.frame(Tahun = integer(), `Konsumsi Listrik` = numeric())
      })
      write.csv(data_listrik, file, row.names = FALSE)
      showNotification("Data Konsumsi Listrik berhasil didownload!", type = "success", duration = 3)
    }
  )
  
  output$quick_download_emisi <- downloadHandler(
    filename = function() {
      paste("data_emisi_grk_indonesia_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataGRK, file, row.names = FALSE)
      showNotification("Data Emisi GRK berhasil didownload!", type = "success", duration = 3)
    }
  )
  
  # Observasi untuk memberikan feedback kepada user tentang dataset
  observe({
    req(input$dataset_preview)
    data <- preview_data()
    
    # Berikan informasi tentang dataset yang dipilih
    dataset_info <- switch(input$dataset_preview,
                           "clustering" = "📊 Dataset Clustering berisi data timbulan sampah, sisa makanan, dan jumlah TPA per provinsi",
                           "pltsa" = "⚡ Dataset PLTSa berisi informasi lokasi dan kapasitas Pembangkit Listrik Tenaga Sampah",
                           "listrik" = "🔌 Dataset Konsumsi Listrik berisi data konsumsi listrik nasional dari tahun 1995-2023",
                           "emisi" = "🌍 Dataset Emisi GRK berisi data emisi gas rumah kaca dari tahun 2000-2019"
    )
  })
  
  # Observasi untuk cek ketersediaan file
  observe({
    missing_files <- c()
    
    if(!file.exists("data/Konsumsi Listrik Nasional.xlsx")) {
      missing_files <- c(missing_files, "Konsumsi Listrik Nasional.xlsx")
    }
    
    if(!file.exists("data/LokasiKapasitasPLTSa(MW).xlsx")) {
      missing_files <- c(missing_files, "LokasiKapasitasPLTSa(MW).xlsx")
    }
    
    if(!file.exists("data/Data Timbulan Sampah.xlsx")) {
      missing_files <- c(missing_files, "Data Timbulan Sampah.xlsx")
    }
    
    if(length(missing_files) > 0) {
      showNotification(
        paste("⚠️ File tidak ditemukan:", paste(missing_files, collapse = ", ")), 
        type = "warning", 
        duration = 5
      )
    }
  })
}
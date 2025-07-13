library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyjs)
library(plotly)
library(DT)
library(shinycssloaders)
library(rsconnect)


ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "♻️ FLowWa", titleWidth = 250),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Clustering", tabName = "clustering", icon = icon("project-diagram")),
      menuItem("Proyeksi Konsumsi Listrik", tabName = "proyeksi_listrik", icon = icon("bolt")),
      menuItem("Proyeksi Emisi GRK", tabName = "forecast_emisi", icon = icon("cloud")),
      menuItem("Konversi FLW jadi Energi", tabName = "konversi", icon = icon("exchange-alt")),
      menuItem("Data PLTSa & TPA", tabName = "pltsa_tpa", icon = icon("solar-panel")),
      menuItem("Analisis Inferensia", icon = icon("wave-square"),
               tabName = "inferensia",
               menuSubItem("Uji Normalitas", tabName = "uji_normalitas"),
               menuSubItem("Uji Korelasi", tabName = "uji_korelasi")
      ),
      menuItem("Metadata", tabName = "metadata", icon = icon("database")),
      menuItem("Tentang Kami", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      useShinyjs(),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML("
        /* ====== Global Background ====== */
        .content-wrapper, .right-side { 
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
          min-height: 100vh;
        }
      
        /* ====== Header & Logo Gradient ====== */
        .main-header .navbar,
        .main-header .logo {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
          border: none;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        .main-header .logo {
          background: linear-gradient(135deg, #6a11cb, #2575fc) !important;
          font-weight: bold;
          font-size: 18px;
          font-family: 'Poppins', sans-serif;
          color: white !important;
          display: flex;
          align-items: center;
          justify-content: center;
          overflow: hidden;
        }
        
        .main-header .logo span {
          background: linear-gradient(135deg, #a18cd1, #fbc2eb);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          font-weight: 600;
          font-size: 18px;
        }
      
        /* ====== Sidebar Menu Styling ====== */
        .main-sidebar {
          background: linear-gradient(180deg, #2c3e50 0%, #3b3f54 100%) !important;
          box-shadow: 2px 0 12px rgba(0,0,0,0.2);
          border-top-right-radius: 20px;
          border-bottom-right-radius: 20px;
        }
        
        /* ===== Sidebar Menu Item ===== */
        .sidebar-menu > li > a {
          color: #ecf0f1 !important;
          font-weight: 500;
          font-size: 14px;
          padding: 12px 20px;
          margin: 6px 10px;
          border-radius: 10px;
          transition: all 0.3s ease;
          display: flex;
          align-items: center;
        }
        
        .sidebar-menu > li > a:hover {
          background: rgba(255, 255, 255, 0.08) !important;
          color: #ffffff !important;
          transform: translateX(5px);
        }
        
        /* ===== Sidebar Icon Size Adjustment ===== */
        .sidebar-menu > li > a > i {
          margin-right: 10px;
          font-size: 16px;
        }
        
        /* ===== Active Menu Highlight ===== */
        .sidebar-menu > li.active > a {
          background: linear-gradient(135deg, #8e44ad 0%, #6c5ce7 100%) !important;
          color: #ffffff !important;
          font-weight: 600;
          border-left: 5px solid #ff6b6b;
          box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        }
        
        /* ===== Optional: Submenu ===== */
        .sidebar-menu .treeview-menu > li > a {
          padding-left: 30px;
          font-size: 13px;
          color: #bdc3c7 !important;
        }
        
        .sidebar-menu .treeview-menu > li > a:hover {
          color: #ffffff !important;
        }

      
        /* ====== Text Gradient (for h2 titles) ====== */
        .text-gradient {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
        }
      
        /* ====== Card Icon Bulat (jika digunakan) ====== */
        .card-icon {
          width: 60px;
          height: 60px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-bottom: 20px;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          font-size: 24px;
        }
      
        /* ====== Fade In & Slide In Animation (opsional) ====== */
        .fade-in {
          animation: fadeIn 0.8s ease-in;
        }
      
        .slide-in-left {
          animation: slideInLeft 0.8s ease-out;
        }
      
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
      
        @keyframes slideInLeft {
          from { opacity: 0; transform: translateX(-30px); }
          to { opacity: 1; transform: translateX(0); }
        }
      
        /* ====== Typography Customization ====== */
        h1, h2, h3, h4, h5, h6 {
          font-weight: 600 !important;
          margin-bottom: 20px !important;
          color: #2c3e50 !important;
        }
      
        h1 {
          font-size: 2.5rem !important;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
        }
      
        /* ====== Datatable Styling (jika menggunakan DT) ====== */
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          color: #2c3e50;
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        
        /* Box Styles */
        .box {
          border-radius: 12px !important;
          box-shadow: 0 4px 20px rgba(0,0,0,0.08) !important;
          border: none !important;
          margin-bottom: 25px;
          transition: transform 0.3s ease, box-shadow 0.3s ease;
          background: white;
        }
        
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 30px rgba(0,0,0,0.15) !important;
        }
        
        .box-header {
          border-radius: 12px 12px 0 0 !important;
          padding: 20px !important;
        }
        
        .box-header.with-border {
          border-bottom: 2px solid #ecf0f1 !important;
        }
        
        .box-title {
          font-size: 18px !important;
          font-weight: 600 !important;
          display: flex;
          align-items: center;
        }
        
        .box-title i {
          margin-right: 10px;
          font-size: 20px;
        }
        
        /* Value Box Styles */
        .small-box {
          border-radius: 15px !important;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1) !important;
          transition: all 0.3s ease;
          overflow: hidden;
          position: relative;
        }
        
        .small-box:hover {
          transform: translateY(-8px);
          box-shadow: 0 12px 40px rgba(0,0,0,0.2) !important;
        }
        
        .small-box:before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 4px;
          background: linear-gradient(90deg, #ff6b6b, #4ecdc4, #45b7d1);
        }
        
        .small-box .inner {
          padding: 25px 20px !important;
        }
        
        .small-box h3 {
          font-size: 32px !important;
          font-weight: 700 !important;
          margin: 0 0 10px 0 !important;
          color: white !important;
        }
        
        .small-box p {
          font-size: 16px !important;
          font-weight: 500 !important;
          margin: 0 !important;
          color: white !important;
        }
        
        .small-box .icon {
          font-size: 70px !important;
          opacity: 0.3 !important;
        }
        
        /* Button Styles */
        .btn {
          border-radius: 25px !important;
          padding: 10px 25px !important;
          font-weight: 600 !important;
          border: none !important;
          transition: all 0.3s ease;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        
        .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 5px 15px rgba(0,0,0,0.2);
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        }
        
        .btn-success {
          background: linear-gradient(135deg, #56ab2f 0%, #a8e6cf 100%) !important;
        }
        
        .btn-warning {
          background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%) !important;
        }
        
        .btn-info {
          background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%) !important;
        }
        
        /* Form Controls */
        .form-control, .selectize-input {
          border-radius: 10px !important;
          border: 2px solid #e9ecef !important;
          padding: 12px 15px !important;
          transition: all 0.3s ease;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #667eea !important;
          box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25) !important;
        }
        
        /* Typography */
        h1, h2, h3, h4, h5, h6 {
          color: #2c3e50 !important;
          font-weight: 600 !important;
          margin-bottom: 20px !important;
        }
        
        h1 {
          font-size: 2.5rem !important;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
        }
        
        /* Welcome Section */
        .welcome-section {
          background: linear-gradient(135deg, rgba(102, 126, 234, 0.1) 0%, rgba(118, 75, 162, 0.1) 100%);
          border-radius: 20px;
          padding: 40px;
          margin-bottom: 30px;
          text-align: center;
          position: relative;
          overflow: hidden;
        }
        
        .welcome-section:before {
          content: '';
          position: absolute;
          top: -50%;
          left: -50%;
          width: 200%;
          height: 200%;
          background: radial-gradient(circle, rgba(102, 126, 234, 0.05) 0%, transparent 70%);
          animation: pulse 4s ease-in-out infinite;
        }
        
        @keyframes pulse {
          0%, 100% { transform: scale(1); }
          50% { transform: scale(1.05); }
        }
      "))
    ),
    
    tabItems(
      tabItem("home",
              
              # Header sambutan
              fluidRow(
                column(12,
                       div(class = "welcome-section fade-in",
                           tags$h1("Selamat Datang di Dashboard Food Loss and Waste (FLW) Indonesia", 
                                   style = "
                       font-size: 2.5rem;
                       font-weight: 800;
                       text-align: center;
                       background: linear-gradient(135deg, #7b2ff7, #f107a3);
                       -webkit-background-clip: text;
                       -webkit-text-fill-color: transparent;
                       margin-bottom: 10px;
                     "),
                           tags$p("Pantau, dan analisis untuk mengurangi Food Loss and Waste di Indonesia.", 
                                  style = "
                      text-align: center;
                      font-size: 18px;
                      color: #555;
                      font-style: italic;
                      margin-top: 0px;
                      margin-bottom: 25px;
                    ")
                       )
                )
              ),
              
              # Kotak ringkasan data utama
              fluidRow(
                valueBoxOutput("sampahBox", width = 4),
                valueBoxOutput("listrikBox", width = 4),
                valueBoxOutput("emisiBox", width = 4)
              ),
              
              # Ringkasan narasi
              fluidRow(
                box(
                  title = tagList(icon("info-circle", style="color: white;"), 
                                  span("Ringkasan Dashboard", style = "color: white;")), 
                  status = "primary", solidHeader = TRUE, width = 12,
                  class = "fade-in",
                  div(style = "font-size:16px; line-height:1.8; color:#2c3e50; padding: 10px 15px;",
                      tags$p(HTML("Indonesia menghadapi tantangan besar terkait <strong>Food Loss and Waste (FLW)</strong>. Setiap tahun, sekitar 
                    <span style='color:#e84393; font-weight:bold;'>48 juta ton makanan</span> terbuang sia-sia, 
                    setara dengan <span style='color:#6c5ce7; font-weight:bold;'>4–5% dari PDB nasional</span>.")),
                      
                      tags$p(HTML("Ironisnya, sekitar <span style='color:#d63031; font-weight:bold;'>20 juta orang</span> mengalami kekurangan gizi dan kelaparan. 
                     Sebagian besar limbah makanan ini berakhir di tempat pembuangan akhir dan menghasilkan gas metana (CH₄) 
                     yang berpotensi memanaskan bumi <span style='color:#d63031; font-weight:bold;'>25x lebih kuat</span> dibanding CO₂.")),
                      
                      tags$p(HTML("Selain itu, konsumsi energi Indonesia masih didominasi sumber fosil seperti batu bara dan minyak bumi (>50%). 
                    FLW memiliki potensi besar untuk diolah menjadi <span style='color:#00cec9; font-weight:bold;'>energi terbarukan</span> 
                    lewat teknologi <span style='color:#0984e3; font-weight:bold;'>Pembangkit Listrik Tenaga Sampah (PLTSa)</span>, 
                    meskipun pemanfaatannya saat ini masih sangat terbatas."))
                  )
                )
              ),
              
              # Fakta penting singkat
              fluidRow(
                box(width = 4, status = "info", solidHeader = TRUE,
                    class = "fade-in",
                    div(style = "text-align: center; padding: 15px;",
                        tags$div("💡", style = "font-size: 48px; margin-bottom: 10px;"),
                        tags$h4("Fakta", style = "font-weight: bold; color: #0984e3;"),
                        tags$p("Sekitar", tags$strong("48 juta ton makanan"), "terbuang setiap tahun di Indonesia.",
                               style = "font-size: 15px; color: #2d3436;")
                    )
                ),
                
                box(width = 4, status = "success", solidHeader = TRUE,
                    class = "fade-in",
                    div(style = "text-align: center; padding: 15px;",
                        tags$div("⚠", style = "font-size: 48px; margin-bottom: 10px;"),
                        tags$h4("Dampak", style = "font-weight: bold; color: #00b894;"),
                        tags$p("Gas metana dari FLW", tags$strong("25x lebih kuat"), "dari CO₂ dalam memicu pemanasan global.",
                               style = "font-size: 15px; color: #2d3436;")
                    )
                ),
                
                box(width = 4, status = "warning", solidHeader = TRUE,
                    class = "fade-in",
                    div(style = "text-align: center; padding: 15px;",
                        tags$div("⚡", style = "font-size: 48px; margin-bottom: 10px;"),
                        tags$h4("Peluang", style = "font-weight: bold; color: #e67e22;"),
                        tags$p("FLW memiliki potensi untuk diubah menjadi", tags$strong("energi terbarukan"),
                               "lewat teknologi PLTSa.", style = "font-size: 15px; color: #2d3436;")
                    )
                )
              ),
              
              # Progress capaian target nasional
              fluidRow(
                box(
                  title = tagList(icon("bullseye", style = "color:white;"), span("Target Nasional Pengurangan FLW 2030", style = "color:white;")),
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  tags$div(
                    style = "font-size:16px; font-weight:500; margin-bottom:10px;",
                    "Pencapaian saat ini terhadap target pengurangan Food Loss and Waste sebesar 50%:"
                  ),
                  tags$div(
                    style = "background:#ecf0f1; border-radius:30px; height:30px; overflow:hidden; box-shadow:inset 0 1px 3px rgba(0,0,0,0.1);",
                    tags$div(
                      style = "
                        width:60%; 
                        background:linear-gradient(90deg, #6a11cb 0%, #2575fc 100%);
                        color:white; 
                        padding:5px 15px; 
                        height:100%;
                        border-radius:30px; 
                        font-weight:bold;
                        display:flex; 
                        align-items:center; 
                        justify-content:flex-start;
                        transition:width 1s ease;
                      ",
                      "60% tercapai"
                    )
                  )
                )
              ),
              
              # Navigasi cepat ke fitur penting
              fluidRow(
                column(12, align = "center",
                       tags$div(style = "margin-bottom: 30px;",
                                actionButton("to_cluster", label = tagList(icon("project-diagram"), " Lihat Clustering"),
                                             class = "btn feature-btn btn-lg", style = "background: linear-gradient(135deg, #00c6ff 0%, #0072ff 100%); color:white; margin: 12px;"),
                                
                                actionButton("to_forecast", label = tagList(icon("cloud"), " Proyeksi Emisi"),
                                             class = "btn feature-btn btn-lg", style = "background: linear-gradient(135deg, #f7971e 0%, #ffd200 100%); color:white; margin: 12px;"),
                                
                                actionButton("to_energy", label = tagList(icon("bolt"), " Potensi Energi"),
                                             class = "btn feature-btn btn-lg", style = "background: linear-gradient(135deg, #56ab2f 0%, #a8e063 100%); color:white; margin: 12px;")
                       )
                )
              ),
              
              # Video edukatif (YouTube)
              fluidRow(
                box(title = tagList(icon("play-circle", style = "color:white;"), span("Video: Apa itu Food Loss and Waste?", style = "color:white;")),
                    width = 12, status = "danger", solidHeader = TRUE,
                    tags$iframe(
                      width = "100%", height = "420",
                      src = "https://www.youtube.com/embed/SRjmEsuTPbk",
                      frameborder = "0", allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                      allowfullscreen = NA,
                      style = "border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.2);"
                    )
                )
              ),
              
              # Video Tutorial
              fluidRow(
                box(title = tagList(icon("play-circle", style = "color:white;"), span("Video: Tutorial Penggunaan Dashboard?", style = "color:white;")),
                    width = 12, status = "danger", solidHeader = TRUE,
                    tags$iframe(
                      width = "100%", height = "420",
                      src = "https://www.youtube.com/embed/M2YYWjItP9Q",  # <-- ini sudah benar
                      frameborder = "0", allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                      allowfullscreen = NA,
                      style = "border-radius: 10px; box-shadow: 0 4px 12px rgba(0,0,0,0.2);"
                    )
                )
              )
      ),
      
      tabItem("clustering",
              fluidRow(
                box(width = 12, title = "Peta Hasil Clustering", leafletOutput("petaCluster", height = 400))
              ),
              
              fluidRow(
                valueBoxOutput("jumlahCluster1"),
                valueBoxOutput("jumlahCluster2"),
                valueBoxOutput("jumlahCluster3")
              ),
              
              fluidRow(
                box(width = 6, title = "Bar Chart Jumlah Wilayah per Cluster", plotOutput("barCluster")),
                box(width = 6, title = "Boxplot Timbulan Sampah per Cluster", plotOutput("boxplotCluster"))
              ),
              
              fluidRow(
                box(width = 12, title = "Radar Chart Profil Cluster",
                    plotOutput("radarCluster"),
                    br(),
                    verbatimTextOutput("interpretasiRadar")
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Tabel Hasil Clustering", DT::dataTableOutput("tabelCluster"))
              ),
              
              fluidRow(
                box(width = 12, uiOutput("insightCluster"))
              )
      ),
      tabItem(
        tabName = "proyeksi_listrik",
        
        # Judul Halaman
        fluidRow(
          box(
            width = 12,
            h2("Proyeksi Konsumsi Listrik", style = "font-weight: bold; color: #2c3e50;")
          )
        ),
        
        # Value Box Ringkasan Data
        fluidRow(
          valueBoxOutput("total_konsumsi"),
          valueBoxOutput("rata_konsumsi"),
          valueBoxOutput("prediksi_tahun")
        ),
        
        fluidRow(
          box(
            title = "Pilih Tahun Prediksi", 
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            sliderInput("tahun_input", "Slider Input Tahun:", min = 2024, max = 2030, value = 2025, step = 1, sep = "")
          )
        ),
        
        # Menu Data dan Analisis 
        fluidRow(
          tabBox(
            title = "Menu",
            width = 12,
            id = "menuId",
            
            # Tabel Data Konsumsi Listrik Nasional
            tabPanel(
              "Data Konsumsi Listrik Nasional",
              box(
                title = "Data Konsumsi Listrik Nasional",
                width = 6,
                status = "primary",
                solidHeader = TRUE,
                DTOutput("tabel_konsumsi")
              )
            ),
            
            # Analisis Regresi Linier
            tabPanel(
              "Analisis Regresi Linier",
              # Plot 
              box(
                title = "Plot Regresi Linier",
                width = 12,
                status = "info", 
                solidHeader = TRUE,
                plotOutput("plot_regresi")
              ),
              # Summary Model & Interpretasi
              box(
                title = "Summary Model",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                htmlOutput("hasil_summary")
              )
            ),
            
            # Tabel Prediksi Konsumsi Listrik Nasional
            tabPanel(
              "Prediksi Konsumsi Listrik Nasional",
              box(
                title = "Prediksi Konsumsi Listrik Nasional",
                width = 6,
                status = "success",
                solidHeader = TRUE,
                DTOutput("tabel_prediksi")
              )
            ),
            
            # Uji Asumsi Klasik
            tabPanel(
              "Uji Asumsi Klasik",
              fluidRow(
                box(
                  title = "Uji Asumsi Error Unbiased",
                  width = 3,
                  status = "warning",
                  solidHeader = TRUE,
                  uiOutput("uji_unbiased")
                ),
                box(
                  title = "Uji Homoskedastisitas",
                  width = 3,
                  status = "warning",
                  solidHeader = TRUE,
                  uiOutput("uji_homoskedastisitas")
                ),
                box(
                  title = "Uji Non-Autokorelasi",
                  width = 3,
                  status = "warning",
                  solidHeader = TRUE,
                  uiOutput("uji_autokorelasi")
                ),
                box(
                  title = "Uji Normalitas",
                  width = 3,
                  status = "warning",
                  solidHeader = TRUE,
                  uiOutput("uji_normalitas")
                )
              )
            )
          )
        )
      ),
      tabItem(tabName = "forecast_emisi",
              
              # Header box
              fluidRow(
                valueBoxOutput("proyeksi_2025"),
                valueBoxOutput("proyeksi_2045"),
                valueBoxOutput("peningkatan_rate")
              ),
              
              # Control box
              fluidRow(
                box(
                  title = tags$div("⚙️ Pengaturan Proyeksi", style = "color: white; font-weight: bold; font-size: 18px;"),
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  div(class = "row",
                      div(class = "col-sm-4",
                          numericInput("tahun_custom", "Masukkan Tahun untuk Proyeksi:", 
                                       value = 2030, min = 2020, max = 2050, step = 1)
                      ),
                      div(class = "col-sm-4",
                          selectInput("show_ci", "Tampilkan Confidence Interval:", 
                                      choices = c("Ya" = TRUE, "Tidak" = FALSE), selected = TRUE)
                      ),
                      div(class = "col-sm-4",
                          selectInput("chart_type", "Jenis Grafik:", 
                                      choices = c("Interaktif" = "plotly", "Statis" = "ggplot"))
                      )
                  )
                )
              ),
              
              # Output Proyeksi Custom
              fluidRow(
                box(
                  title = tags$div(icon("chart-area"), "Hasil Proyeksi Custom", style = "color: white; font-weight: bold; font-size: 18px;"),
                  width = 12, status = "success", solidHeader = TRUE,
                  h4(textOutput("hasil_proyeksi_custom"), style = "margin-top: 10px; font-weight: 600;")
                )
              ),
              
              # Visualisasi utama dan statistik
              fluidRow(
                box(
                  title = tags$div(icon("cloud-showers-heavy"), "Visualisasi Proyeksi Emisi GRK dari Food Loss & Waste", style = "color: white; font-weight: bold; font-size: 16px;"),
                  width = 8, status = "primary", solidHeader = TRUE,
                  withSpinner(
                    conditionalPanel(
                      condition = "input.chart_type == 'plotly'",
                      plotlyOutput("plotEmisiInteraktif", height = "400px")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.chart_type == 'ggplot'",
                    plotOutput("plotEmisiStatis", height = "400px")
                  )
                ),
                box(
                  title = tags$div(icon("calculator"), "Statistik Model Regresi", style = "color: white; font-weight: bold; font-size: 16px;"),
                  width = 4, status = "warning", solidHeader = TRUE,
                  uiOutput("statistik_model_ui")
                )
              ),
              
              # Tabel dan Analisis Dampak
              fluidRow(
                box(
                  title = tags$div(icon("table"), "Tabel Proyeksi Emisi GRK", style = "color: white; font-weight: bold; font-size: 16px;"),
                  width = 6, status = "success", solidHeader = TRUE,
                  DT::dataTableOutput("tabelProyeksi", height = "300px")
                ),
                box(
                  title = tags$div(icon("chart-line"), "Analisis Trend & Dampak", style = "color: white; font-weight: bold; font-size: 16px;"),
                  width = 6, status = "info", solidHeader = TRUE,
                  h4("Trend Emisi:", style = "font-size: 16px; font-weight: 600;"),
                  textOutput("analisis_trend"),
                  br(),
                  h4("Proyeksi Dampak:", style = "font-size: 16px; font-weight: 600;"),
                  textOutput("analisis_dampak"),
                  br(),
                  h4("Rekomendasi:", style = "font-size: 16px; font-weight: 600;"),
                  htmlOutput("rekomendasi")
                )
              ),
              
              # Skenario Mitigasi
              fluidRow(
                box(
                  title = tags$div(icon("balance-scale"), "Perbandingan Skenario Mitigasi", style = "color: white; font-weight: bold; font-size: 16px;"),
                  width = 8, status = "primary", solidHeader = TRUE,
                  plotlyOutput("plotSkenario", height = "350px")
                ),
                box(
                  title = tags$div(icon("info-circle"), "Penjelasan Skenario Mitigasi", style = "color: white; font-weight: bold; font-size: 16px;"),
                  width = 4, status = "info", solidHeader = TRUE,
                  h4("Relevansi:", style = "font-size: 15px; font-weight: 600;"),
                  tags$p("Grafik ini menunjukkan dampak berbagai tingkat upaya mitigasi terhadap proyeksi emisi GRK dari Food Loss & Waste.", 
                         style = "font-size: 13px;"),
                  br(),
                  h4("Penjelasan Skenario:", style = "font-size: 15px; font-weight: 600;"),
                  tags$ul(
                    style = "font-size: 13px; padding-left: 18px;",
                    tags$li(tags$strong("Business as Usual (merah):"), " Proyeksi tanpa intervensi apapun"),
                    tags$li(tags$strong("Mitigasi 10% (tosca):"), " Pengurangan emisi 10% melalui program dasar pengelolaan limbah"),
                    tags$li(tags$strong("Mitigasi 25% (biru):"), " Pengurangan emisi 25% dengan teknologi menengah dan kebijakan terintegrasi"),
                    tags$li(tags$strong("Mitigasi 50% (hijau):"), " Pengurangan emisi 50% dengan teknologi canggih dan transformasi sistem pangan")
                  ),
                  br(),
                  h4("Implikasi:", style = "font-size: 15px; font-weight: 600;"),
                  tags$p("Semakin tinggi tingkat mitigasi, semakin besar dampak positif terhadap pengurangan emisi GRK, namun memerlukan investasi dan komitmen yang lebih besar.", 
                         style = "font-size: 13px;")
                )
              )
      ),
      tabItem("konversi",
              h2(icon("bolt", class = "text-warning"), 
                 span("Konversi FLW menjadi Energi Listrik", 
                      style = "font-weight: bold; color: #4B0082; margin-left: 10px;")
              ),
              
              br(),
              
              fluidRow(
                box(
                  title = tagList(icon("weight-hanging", style = "color:white;"), 
                                  span(" Input Timbulan FLW", style = "color:white; font-weight:600;")),
                  width = 6, status = "primary", solidHeader = TRUE,
                  div(style = "margin-bottom: 10px;",
                      numericInput("input_flw", 
                                   label = "Masukkan berat FLW (dalam ribu ton):", 
                                   value = 0, min = 0)
                  ),
                  actionButton("hitung_konversi", "Hitung Energi Listrik", 
                               icon = icon("calculator"), class = "btn btn-success", 
                               style = "margin-top: 10px; font-weight: bold; padding: 10px 20px; border-radius: 20px;")
                ),
                
                box(
                  title = tagList(icon("bolt", style = "color:white;"), 
                                  span(" Hasil Konversi", style = "color:white; font-weight:600;")),
                  width = 6, status = "success", solidHeader = TRUE,
                  div(class = "highlight-text", 
                      style = "font-size: 28px; font-weight: bold; text-align:center; margin-bottom: 10px;",
                      textOutput("output_energi_number")),
                  div(class = "note-text", 
                      style = "font-size: 14px; color: #6c757d; font-style: italic; text-align:center;",
                      textOutput("output_energi_description"))
                )
              ),
              
              fluidRow(
                box(
                  title = tagList(icon("info-circle", style = "color:white;"), 
                                  span(" Penjelasan Konversi", style = "color:white; font-weight:600;")),
                  width = 12, status = "info", solidHeader = TRUE,
                  HTML("
                    <p style='font-size: 15px;'>
                      Konversi ini didasarkan pada asumsi bahwa setiap <strong>750 ton</strong> timbulan 
                      Food Loss and Waste (FLW) dapat menghasilkan sekitar <strong>474 MWh</strong> energi listrik.
                    </p>
                    <p style='font-size: 15px;'>Perhitungan menggunakan rumus:</p>
                    <div style='background-color:#f8f9fa; padding:10px 15px; border-left:5px solid #007bff; font-family:monospace; font-size: 15px;'>
                      Energi (MWh) = (FLW × 1000 / 750) × 474
                    </div>
                    <p style='font-size: 13px; color: #7f8c8d; font-style: italic;'>
                      Catatan: Input dalam ribu ton akan dikalikan 1000 untuk menjadi ton.
                    </p>
                  ")
                )
              )
      ),
      tabItem(
        tabName = "pltsa_tpa",
        
        # Judul Halaman
        fluidRow(
          box(
            width = 12,
            h2("Data PLTSa & TPA di Indonesia", style = "font-weight: bold; color: #2c3e50;"),
            p("Tabel Data Kapasitas PLTSa dan Jumlah TPA di Berbagai Wilayah", 
              style = "color: #7f8c8d; font-size: 14px;")
          )
        ),
        
        # Value Box Ringkasan Data
        fluidRow(
          valueBoxOutput("total_pltsa", width = 4),
          valueBoxOutput("total_tpa", width = 4),
          valueBoxOutput("prov_terbanyak_tpa", width = 4)
        ),
        
        br(),
        
        # Tabel Data PLTSa dan TPA
        fluidRow(
          box(
            title = tagList(icon("solar-panel", style = "color:white;"), 
                            span(" Data Kapasitas PLTSa", style = "color:white; font-weight:600;")),
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("pltsa_table"),
            footer = tags$p("Sumber: KLHK dan PLN", class = "note-text", style = "margin-top: 10px; font-size: 12px; color: #7f8c8d;")
          ),
          box(
            title = tagList(icon("dumpster", style = "color:white;"), 
                            span(" Data Jumlah TPA per Provinsi", style = "color:white; font-weight:600;")),
            width = 6,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("tpa_table"),
            footer = tags$p("Catatan: Data terbaru 2024", class = "note-text", style = "margin-top: 10px; font-size: 12px; color: #7f8c8d;")
          )
        )
      ),
      tabItem(tabName = "uji_normalitas",
              h2("Uji Normalitas", style = "color:#4B0082; font-weight:bold; text-align:center; margin-bottom:25px;"),
              
              fluidRow(
                box(
                  width = 4,
                  title = tagList(icon("sliders-h"), " Pengaturan Uji Normalitas"),
                  solidHeader = TRUE,
                  style = "background-color: #2c3e50; color: white; border-radius: 10px;",
                  
                  uiOutput("dataset_selector"),
                  uiOutput("var_select"),
                  
                  numericInput("alpha", "Tingkat Signifikansi (α):", 
                               value = 0.05, min = 0.001, max = 1, step = 0.01),
                  
                  selectInput("metode_uji", "Metode Uji Normalitas:",
                              choices = list(
                                "Untuk Sampel Kecil" = list(
                                  "Shapiro-Wilk" = "shapiro",
                                  "Lilliefors" = "lf"
                                ),
                                "Untuk Sampel Besar" = list(
                                  "Goodness of Fit" = "gf",
                                  "Kolmogorov-Smirnov" = "ks",
                                  "Jarque Berra" = "jb"
                                )
                              )),
                  
                  uiOutput("peringatan_validasi", style = "margin-bottom:10px;"),
                  actionButton("run_test", "Jalankan Uji", icon = icon("play"), class = "btn btn-success")
                ),
                
                box(
                  width = 8,
                  title = tagList(icon("check-double"), " Hasil Uji Normalitas"),
                  solidHeader = TRUE,
                  style = "background-color: #34495e; color: white; border-radius: 10px;",
                  
                  fluidRow(
                    box(
                      title = tagList(icon("balance-scale"), " a. Hipotesis"),
                      width = 6,
                      solidHeader = TRUE,
                      style = "background-color: #3B5998; color: white;",
                      uiOutput("hipotesis")
                    ),
                    box(
                      title = tagList(icon("percent"), " b. Tingkat Signifikansi"),
                      width = 6,
                      solidHeader = TRUE,
                      style = "background-color: #3498db; color: white;",
                      textOutput("sig_level")
                    )
                  ),
                  
                  fluidRow(
                    box(
                      title = tagList(icon("calculator"), " c. Hasil Statistik Uji"),
                      width = 12,
                      solidHeader = TRUE,
                      style = "background-color: #f39c12; color: white;",
                      uiOutput("stat_uji")
                    )
                  ),
                  
                  fluidRow(
                    box(
                      title = tagList(icon("gavel"), " d. Keputusan"),
                      width = 6,
                      solidHeader = TRUE,
                      style = "background-color: #27ae60; color: white;",
                      textOutput("keputusan")
                    ),
                    box(
                      title = tagList(icon("comment-alt"), " e. Kesimpulan"),
                      width = 6,
                      solidHeader = TRUE,
                      style = "background-color: #e74c3c; color: white;",
                      textOutput("kesimpulan")
                    )
                  ),
                  
                  fluidRow(
                    box(
                      title = tagList(icon("chart-bar"), " Visualisasi Histogram"),
                      width = 12,
                      solidHeader = TRUE,
                      style = "background-color: #16a085; color: white;",
                      plotOutput("hist_plot_normal")
                    )
                  )
                )
              )
      ),
      
      tabItem(tabName = "uji_korelasi",
              h2("Uji Korelasi", style = "color:#4B0082; font-weight:bold; text-align:center; margin-bottom:25px;"),
              
              fluidRow(
                box(
                  width = 4,
                  title = tagList(icon("sliders-h"), " Pengaturan Uji Korelasi"),
                  solidHeader = TRUE,
                  style = "background-color: #2c3e50; color: white; border-radius: 10px;",
                  uiOutput("dataset_selector_korelasi"),
                  uiOutput("var_x_select"),
                  uiOutput("var_y_select"),
                  numericInput("alpha_korelasi", "Tingkat Signifikansi (α):", 
                               value = 0.05, min = 0.001, max = 1, step = 0.01),
                  selectInput("metode_korelasi", "Metode Korelasi:",
                              choices = list(
                                "Pearson (linear, data normal)" = "pearson",
                                "Spearman (non-parametrik)" = "spearman",
                                "Kendall (non-parametrik)" = "kendall"
                              )),
                  uiOutput("peringatan_validasi_korelasi"),
                  tags$div(style = "margin-top:10px;"),
                  actionButton("run_korelasi", " Jalankan Uji", icon = icon("play"), class = "btn btn-success btn-block")
                ),
                
                box(
                  width = 8,
                  title = tagList(icon("chart-line"), " Hasil Uji Korelasi"),
                  solidHeader = TRUE,
                  style = "background-color: #34495e; color: white; border-radius: 10px;",
                  
                  fluidRow(
                    box(
                      title = tagList(icon("balance-scale-left"), " a. Hipotesis"),
                      width = 6,
                      solidHeader = TRUE,
                      style = "background-color: #3B5998; color: white;",
                      uiOutput("hipotesis_korelasi")
                    ),
                    box(
                      title = tagList(icon("percent"), " b. Tingkat Signifikansi"),
                      width = 6,
                      solidHeader = TRUE,
                      style = "background-color: #3498db; color: white;",
                      textOutput("sig_level_korelasi")
                    )
                  ),
                  
                  fluidRow(
                    box(
                      title = tagList(icon("calculator"), " c. Hasil Statistik Uji"),
                      width = 12,
                      solidHeader = TRUE,
                      style = "background-color: #f39c12; color: white;",
                      uiOutput("stat_uji_korelasi")
                    )
                  ),
                  
                  fluidRow(
                    box(
                      title = tagList(icon("gavel"), " d. Keputusan"),
                      width = 6,
                      solidHeader = TRUE,
                      style = "background-color: #27ae60; color: white;",
                      textOutput("keputusan_korelasi")
                    ),
                    box(
                      title = tagList(icon("comment-alt"), " e. Kesimpulan"),
                      width = 6,
                      solidHeader = TRUE,
                      style = "background-color: #e74c3c; color: white;",
                      textOutput("kesimpulan_korelasi")
                    )
                  ),
                  
                  fluidRow(
                    box(
                      title = tagList(icon("chart-scatter"), " Visualisasi Scatter Plot"),
                      width = 12,
                      solidHeader = TRUE,
                      style = "background-color: #16a085; color: white;",
                      plotOutput("scatter_plot_korelasi")
                    )
                  )
                )
              )
      ),
      
      tabItem(tabName = "metadata",
              # CSS DataTable yang Diperbaiki (Tanpa Duplikasi)
              tags$head(
                tags$style(HTML("
            /* DataTable Wrapper Styling */
            .dataTables_wrapper {
              background: white;
              border-radius: 10px;
              padding: 15px;
              box-shadow: 0 4px 15px rgba(0,0,0,0.1);
            }
            
            /* Control Elements */
            .dataTables_wrapper .dataTables_length,
            .dataTables_wrapper .dataTables_filter,
            .dataTables_wrapper .dataTables_info {
              color: #2c3e50;
              font-weight: 500;
              margin-bottom: 10px;
            }
            
            /* Search Input */
            .dataTables_wrapper .dataTables_filter input {
              border: 2px solid #3498db;
              border-radius: 8px;
              padding: 8px 12px;
              margin-left: 8px;
              transition: border-color 0.3s ease;
            }
            
            .dataTables_wrapper .dataTables_filter input:focus {
              border-color: #2980b9;
              outline: none;
              box-shadow: 0 0 5px rgba(52, 152, 219, 0.3);
            }
            
            /* Length Select */
            .dataTables_wrapper .dataTables_length select {
              border: 2px solid #3498db;
              border-radius: 8px;
              padding: 5px 10px;
              margin: 0 8px;
            }
            
            /* Table Header */
            .dataTables_wrapper table.dataTable thead th {
              background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
              color: white;
              font-weight: 600;
              text-align: center;
              border: none;
              padding: 15px 10px;
            }
            
            /* Table Body */
            .dataTables_wrapper table.dataTable tbody td {
              padding: 12px 10px;
              border-bottom: 1px solid #ecf0f1;
              text-align: center;
              vertical-align: middle;
            }
            
            .dataTables_wrapper table.dataTable tbody tr:hover {
              background-color: #f8f9fa;
            }
            
            .dataTables_wrapper table.dataTable tbody tr:nth-child(even) {
              background-color: #fdfdfd;
            }
            
            /* Pagination - STYLING SEDERHANA TANPA DUPLIKASI */
            .dataTables_wrapper .dataTables_paginate {
              text-align: center;
              margin-top: 15px;
            }
            
            .dataTables_wrapper .dataTables_paginate .paginate_button {
              border: 1px solid #3498db !important;
              background: white !important;
              color: #3498db !important;
              margin: 0 2px;
              border-radius: 6px;
              padding: 6px 12px;
              font-weight: 500;
              transition: all 0.3s ease;
            }
            
            .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
              background: #3498db !important;
              color: white !important;
            }
            
            .dataTables_wrapper .dataTables_paginate .paginate_button.current {
              background: #3498db !important;
              color: white !important;
              font-weight: bold;
            }
            
            .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
              color: #bdc3c7 !important;
              border-color: #bdc3c7 !important;
              cursor: not-allowed;
            }
            
            /* Info Text */
            .dataTables_wrapper .dataTables_info {
              padding-top: 15px;
              font-size: 14px;
              color: #7f8c8d;
            }
            
            /* Preview Header */
            .preview-header {
              background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
              color: white;
              padding: 20px;
              border-radius: 10px 10px 0 0;
              margin-bottom: 0;
            }
            
            /* Dataset Info Cards */
            .dataset-info-card {
              background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
              color: white;
              padding: 15px;
              border-radius: 10px;
              text-align: center;
              margin-bottom: 15px;
              box-shadow: 0 4px 15px rgba(0,0,0,0.1);
              transition: transform 0.3s ease;
            }
            
            .dataset-info-card:hover {
              transform: translateY(-2px);
            }
            
            .dataset-info-card h4 {
              margin: 0 0 5px 0;
              font-size: 24px;
              font-weight: bold;
            }
            
            .dataset-info-card p {
              margin: 0;
              font-size: 14px;
              opacity: 0.9;
            }
          "))
              ),
              
              # Judul Halaman
              fluidRow(
                box(
                  width = 12,
                  h2("📊 Metadata & Download Dataset", style = "font-weight: bold; color: #2c3e50; text-align: center;"),
                  p("Informasi detail mengenai setiap variabel yang digunakan dalam dashboard, serta fitur download dataset untuk analisis lebih lanjut", 
                    style = "color: #7f8c8d; font-size: 16px; text-align: center;")
                )
              ),
              
              # Bagian Download Data dengan Preview yang Diperbaiki
              fluidRow(
                box(
                  title = span("📥 Download & Preview Dataset", style = "color:white; font-weight:600; font-size: 18px;"),
                  status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE,
                  
                  # Header untuk kontrol
                  div(class = "preview-header",
                      h4("🔍 Pilih dan Preview Dataset", style = "margin: 0; text-align: center;")
                  ),
                  
                  # Kontrol untuk memilih dataset
                  fluidRow(style = "margin-top: 20px;",
                           column(4,
                                  selectInput("dataset_preview", 
                                              label = tags$strong("📋 Pilih Dataset:", style = "color: #2c3e50;"),
                                              choices = list(
                                                "🗂️ Data Clustering" = "clustering",
                                                "⚡ Data PLTSa" = "pltsa", 
                                                "🔌 Data Konsumsi Listrik" = "listrik",
                                                "🌍 Data Emisi GRK" = "emisi"
                                              ),
                                              selected = "clustering")
                           ),
                           column(4,
                                  selectInput("download_format", 
                                              label = tags$strong("📄 Format Download:", style = "color: #2c3e50;"),
                                              choices = list(
                                                "📊 CSV (Comma Separated)" = "csv",
                                                "📈 Excel (.xlsx)" = "xlsx"
                                              ))
                           ),
                           column(4,
                                  tags$label(tags$strong("⬇️ Download:", style = "color: #2c3e50;")),
                                  br(),
                                  downloadButton("download_selected", 
                                                 label = tagList(icon("download"), "Download Dataset"), 
                                                 class = "btn btn-success btn-block",
                                                 style = "font-weight: bold; padding: 10px; border-radius: 8px;")
                           )
                  ),
                  
                  # Informasi Dataset yang Dipilih
                  fluidRow(style = "margin-top: 20px;",
                           column(3,
                                  div(class = "dataset-info-card",
                                      h4(htmlOutput("dataset_rows_text"), style = "color: white; margin: 0 0 5px 0; font-size: 24px; font-weight: bold;"),
                                      p("📊 Total Baris Data", style = "color: white; margin: 0; font-size: 14px; opacity: 0.9;")
                                  )
                           ),
                           column(3,
                                  div(class = "dataset-info-card",
                                      h4(htmlOutput("dataset_cols_text"), style = "color: white; margin: 0 0 5px 0; font-size: 24px; font-weight: bold;"),
                                      p("🗂️ Jumlah Kolom", style = "color: white; margin: 0; font-size: 14px; opacity: 0.9;")
                                  )
                           ),
                           column(3,
                                  div(class = "dataset-info-card",
                                      h4(htmlOutput("dataset_numeric_text"), style = "color: white; margin: 0 0 5px 0; font-size: 24px; font-weight: bold;"),
                                      p("🔢 Kolom Numerik", style = "color: white; margin: 0; font-size: 14px; opacity: 0.9;")
                                  )
                           ),
                           column(3,
                                  div(class = "dataset-info-card",
                                      h4(htmlOutput("dataset_missing_text"), style = "color: white; margin: 0 0 5px 0; font-size: 24px; font-weight: bold;"),
                                      p("❓ Data Hilang", style = "color: white; margin: 0; font-size: 14px; opacity: 0.9;")
                                  )
                           )
                  ),
                  
                  # Quick Download Section dengan styling yang lebih baik
                  HTML('<div style="margin-top: 30px; padding: 20px; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 10px;">
                    <h4 style="margin-bottom: 15px; color: #2c3e50; text-align: center;"><i class="fa fa-rocket"></i> Quick Download All Datasets:</h4>'),
                  fluidRow(
                    column(3,
                           downloadButton("quick_download_clustering", 
                                          label = tagList(icon("download"), "Clustering Data"), 
                                          class = "btn btn-info btn-block",
                                          style = "margin-bottom: 10px; font-weight: bold; padding: 12px; border-radius: 8px;")
                    ),
                    column(3,
                           downloadButton("quick_download_pltsa", 
                                          label = tagList(icon("download"), "PLTSa Data"), 
                                          class = "btn btn-warning btn-block",
                                          style = "margin-bottom: 10px; font-weight: bold; padding: 12px; border-radius: 8px;")
                    ),
                    column(3,
                           downloadButton("quick_download_listrik", 
                                          label = tagList(icon("download"), "Listrik Data"), 
                                          class = "btn btn-success btn-block",
                                          style = "margin-bottom: 10px; font-weight: bold; padding: 12px; border-radius: 8px;")
                    ),
                    column(3,
                           downloadButton("quick_download_emisi", 
                                          label = tagList(icon("download"), "Emisi GRK Data"), 
                                          class = "btn btn-danger btn-block",
                                          style = "margin-bottom: 10px; font-weight: bold; padding: 12px; border-radius: 8px;")
                    )
                  ),
                  HTML('</div>'),
                  
                  # Preview Table dengan header yang lebih menarik
                  div(style = "margin-top: 30px;",
                      h4("👀 Preview Dataset Terpilih:", style = "color: #2c3e50; margin-bottom: 15px; text-align: center;"),
                      withSpinner(
                        DT::dataTableOutput("preview_table"),
                        color = "#3498db",
                        type = 4,
                        size = 1
                      )
                  ),
                  
                  # Footer dengan informasi tambahan
                  HTML('<div style="margin-top: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #3498db;">
                    <p style="margin: 0; font-size: 14px; color: #495057;">
                      <i class="fa fa-info-circle" style="color: #3498db;"></i> 
                      <strong>Catatan:</strong> Format CSV kompatibel dengan Excel, R, Python, dan software analisis data lainnya. 
                      Format Excel (.xlsx) mempertahankan formatting asli dan dapat dibuka langsung dengan Microsoft Excel.
                    </p>
                  </div>')
                )
              ),
              
              # Metadata Cards dengan layout yang diperbaiki
              fluidRow(
                column(12,
                       h3("📋 Informasi Detail Variabel Dataset", 
                          style = "margin-top: 40px; margin-bottom: 25px; color: #ffffff; text-align: center; font-weight: bold;")
                )
              ),
              
              fluidRow(
                box(
                  title = span("🥗 Sisa Makanan", style = "color:white; font-weight:600;"),
                  status = "primary", solidHeader = TRUE, width = 4,
                  HTML('
              <table style="width:100%; border-collapse: collapse; font-size: 14px;">
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Nama Variabel</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Sisa Makanan</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Satuan</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Persen (%)</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Periode</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">2024</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Sumber Data</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Sistem Informasi Pengelolaan Sampah Nasional – Kementrian Lingkungan Hidup</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Definisi</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Persentase perkiraan porsi timbulan sampah organik berupa sisa makanan yang dihasilkan oleh rumah tangga dan aktivitas konsumsi lainnya dalam satu tahun</td>
                </tr>
              </table>
            ')
                ),
                
                box(
                  title = span("🗑 Jumlah TPA", style = "color:white; font-weight:600;"),
                  status = "primary", solidHeader = TRUE, width = 4,
                  HTML('
              <table style="width:100%; border-collapse: collapse; font-size: 14px;">
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Nama Variabel</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Jumlah TPA</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Satuan</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Unit</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Periode</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">2024</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Sumber Data</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Sistem Informasi Pengelolaan Sampah Nasional – Kementrian Lingkungan Hidup</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Definisi</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Jumlah total Tempat Pembuangan Akhir (TPA) yang aktif digunakan untuk penampungan dan pengelolaan sampah di tingkat kota/kabupaten</td>
                </tr>
              </table>
            ')
                ),
                box(
                  title = span("🚮 Timbulan Sampah Tahunan", style = "color:white; font-weight:600;"),
                  status = "primary", solidHeader = TRUE, width = 4,
                  HTML('
              <table style="width:100%; border-collapse: collapse; font-size: 14px;">
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Nama Variabel</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Timbulan Sampah Tahunan</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Satuan</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Ton</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Periode</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">2024</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Sumber Data</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Sistem Informasi Pengelolaan Sampah Nasional – Kementrian Lingkungan Hidup</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Definisi</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Total volume atau berat sampah yang dihasilkan oleh seluruh sumber timbulan (rumah tangga, komersial, industri, publik) dalam satu tahun di suatu wilayah</td>
                </tr>
              </table>
            ')
                )
              ),
              fluidRow(
                box(
                  title = span("⚡ Konsumsi Tenaga Listrik", style = "color:white; font-weight:600;"),
                  status = "primary", solidHeader = TRUE, width = 4,
                  HTML('
              <table style="width:100%; border-collapse: collapse; font-size: 14px;">
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Nama Variabel</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Konsumsi Tenaga Listrik</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Satuan</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">GWh (Gigawatt hour)</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Periode</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">1995-2023</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Sumber Data</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Statistik Ketenagalistrikan – Direktorat Jenderal Ketenagalistrikan</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Definisi</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Total energi listrik yang digunakan oleh seluruh sektor pengguna (rumah tangga, industri, bisnis, pemerintah, dan lain-lain) pada tingkat nasional dalam periode waktu tertentu</td>
                </tr>
              </table>
            ')
                ),
                box(
                  title = span("🌏 Emisi Gas Rumah Kaca", style = "color:white; font-weight:600;"),
                  status = "primary", solidHeader = TRUE, width = 4,
                  HTML('
              <table style="width:100%; border-collapse: collapse; font-size: 14px;">
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Nama Variabel</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Emisi Gas Rumah Kaca</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Satuan</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Mton CO₂e (Megaton CO₂ equivalent)</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Periode</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">2000-2019</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Sumber Data</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Laporan Inventarisasi Gas Rumah Kaca dan MPV 2020 – Kementerian Lingkungan Hidup dan Kehutanan</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Definisi</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Total emisi gas-gas yang berkontribusi pada efek rumah kaca (seperti CO₂, CH₄, N₂O) yang dilepaskan dari aktivitas domestik, energi, industri, dan pengelolaan sampah di tingkat nasional</td>
                </tr>
              </table>
            ')
                ),
                box(
                  title = span("🔋 Kapasitas PLTSa", style = "color:white; font-weight:600;"),
                  status = "primary", solidHeader = TRUE, width = 4,
                  HTML('
              <table style="width:100%; border-collapse: collapse; font-size: 14px;">
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Nama Variabel</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Kapasitas PLTSa</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Satuan</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">MW (Megawatt)</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Periode</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">2023</td>
                </tr>
                <tr>
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Sumber Data</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Sistem Informasi Pengelolaan Sampah Nasional – Kementrian Lingkungan Hidup</td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                  <td style="font-weight:bold; padding: 8px; border: 1px solid #dee2e6;">Definisi</td>
                  <td style="padding: 8px; border: 1px solid #dee2e6;">Kapasitas terpasang Pembangkit Listrik Tenaga Sampah (PLTSa) yang dapat mengkonversi sampah menjadi energi listrik dengan output daya listrik terukur dalam Megawatt</td>
                </tr>
              </table>
            ')
                )
              )
      ),
      
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            title = tagList(icon("users"), span("Tentang Kami", style = "color: #6a1b9a; font-weight: bold;")),
            solidHeader = TRUE,
            style = "background: linear-gradient(135deg, #fbe9f9, #e1bee7); color: #4a148c; border-radius: 15px;",
            HTML("
              <div style='text-align: center; margin-bottom: 30px;'>
                <h3 style='color: #ab47bc; font-weight: bold;'>Kelompok 1 Komputasi Statistika Kelas 2KS2</h3>
                <p style='font-size: 16px; color: #6a1b9a;'>Kami adalah mahasiswa dari Politeknik Statistika STIS yang sedang mengembangkan dashboard interaktif untuk uji statistik berbasis R Shiny, sebagai bagian dari Proyek Komputasi Statistik.</p>
              </div>
      
              <div style='display: flex; justify-content: center; gap: 30px; flex-wrap: wrap;'>
                <!-- Card 1 -->
                <div style='background-color: #f3e5f5; border-radius: 15px; padding: 20px; width: 240px; text-align: center; box-shadow: 0 4px 15px rgba(170, 102, 204, 0.3);'>
                  <img src='Avita.jpg' alt='Anggota 1' style='width: 100px; height: 100px; border-radius: 50%; object-fit: cover; border: 3px solid #ba68c8;'>
                  <h4 style='color: #8e24aa; margin-top: 15px;'>Avita Mumtahana</h4>
                  <p style='color: #6a1b9a;'>NIM: 222313008</p>
                </div>
      
                <!-- Card 2 -->
                <div style='background-color: #f3e5f5; border-radius: 15px; padding: 20px; width: 240px; text-align: center; box-shadow: 0 4px 15px rgba(170, 102, 204, 0.3);'>
                  <img src='Udin.jpg' alt='Anggota 2' style='width: 100px; height: 100px; border-radius: 50%; object-fit: cover; border: 3px solid #ba68c8;'>
                  <h4 style='color: #8e24aa; margin-top: 15px;'>Burhanudin</h4>
                  <p style='color: #6a1b9a;'>NIM: 1234567891</p>
                </div>
      
                <!-- Card 3 -->
                <div style='background-color: #f3e5f5; border-radius: 15px; padding: 20px; width: 240px; text-align: center; box-shadow: 0 4px 15px rgba(170, 102, 204, 0.3);'>
                  <img src='Fakhri.jpg' alt='Anggota 3' style='width: 100px; height: 100px; border-radius: 50%; object-fit: cover; border: 3px solid #ba68c8;'>
                  <h4 style='color: #8e24aa; margin-top: 15px;'>Fakhri Iqbrar</h4>
                  <p style='color: #6a1b9a;'>NIM: 222313076</p>
                </div>
              </div>
      
              <hr style='border-top: 1px solid #ce93d8; margin: 40px 0 20px;'>
      
              <div style='text-align: center;'>
                <p style='color: #6a1b9a; font-size: 16px;'>Project ini bernama <b>FLowWa</b>, yaitu dashboard yang membantu pengguna melakukan uji-uji statistik seperti uji normalitas, korelasi, dan lainnya secara visual dan interaktif.</p>
                <p style='font-style: italic; color: #8e24aa;'>Harapan kami, aplikasi ini dapat bermanfaat untuk edukasi dan penelitian.</p>
              </div>
            ")
          )
        )
      )
    )
  )
)

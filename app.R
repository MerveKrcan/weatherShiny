# app.R
library(shiny)
#Shiny sayfa teması
library(shinythemes)
#Api istekleri atmak için gerekli
library(httr)
#Görselleştirme kütüphanesi
library(ggplot2)

# List of 81 Turkish provinces
turkish_provinces <- c("İstanbul", "İzmir", "Adana", "Adıyaman", "Afyonkarahisar", "Ağrı", "Amasya", "Ankara", "Antalya", "Artvin",
                       "Bilecik", "Bingöl", "Bitlis", "Bolu", "Burdur", "Bursa", "Çanakkale", "Çankırı", "Çorum", "Denizli", 
                       "Diyarbakır", "Edirne", "Elazığ", "Erzincan", "Erzurum", "Eskişehir", "Gaziantep", "Giresun", "Gümüşhane",
                       "Hakkâri", "Hatay", "Isparta", "Mersin", "Kars", "Kastamonu", "Kayseri", "Kırklareli", "Aydın", "Balıkesir",
                       "Kırşehir", "Kocaeli", "Konya", "Kütahya", "Malatya", "Manisa", "Kahramanmaraş", "Mardin", "Muğla", "Muş",
                       "Nevşehir", "Niğde", "Ordu", "Rize", "Sakarya", "Samsun", "Siirt", "Sinop", "Sivas", "Tekirdağ", "Tokat", 
                       "Trabzon", "Tunceli", "Şanlıurfa", "Uşak", "Van", "Yozgat", "Zonguldak", "Aksaray", "Bayburt", "Karaman",
                       "Kırıkkale", "Batman", "Şırnak", "Bartın", "Ardahan", "Iğdır", "Yalova", "Karabük", "Kilis", "Osmaniye", "Düzce")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(HTML("<img src='https://cdn.jim-nielsen.com/ios/512/weather-2021-12-07.png' alt='Resim' width='100' height='100' style='border-radius: 8px;' /> <strong>Weather App</strong>")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_city", "Select City", choices = turkish_provinces),
      actionButton("get_weather", "Get Weather", icon = icon("cloud-meatball")),
      actionButton("get_3day_forecast_weather", "Get 2 Days Forecast Weather"),
      br(),
    ),
    
    mainPanel(
      textOutput("error_message"),
      uiOutput("weather_info"),
      tableOutput("forecast_table"),
      plotOutput("temperaturePlot")
    )
  ), 
)

server <- function(input, output) {
  weather_data <- reactiveVal(NULL)
  weather_forecast_data <- reactiveVal(NULL)
  error_message <- reactiveVal(NULL)
  
  # ComboBox değeri değiştirilince Verileri temizlemek için
  observeEvent(input$selected_city, {
    weather_data(NULL)
    weather_forecast_data(NULL)
  })
  
  observeEvent(input$get_weather, {
    req(input$selected_city)  
    
    api_url <- paste0("https://weatherapi-com.p.rapidapi.com/current.json?q=", input$selected_city)
    api_headers <- c(
      'X-RapidAPI-Key' = 'f0fee14d81msh4a61ea9554c3eebp19f257jsn98e5da8f8c90',
      'X-RapidAPI-Host' = 'weatherapi-com.p.rapidapi.com'
    )
    
    response <- tryCatch(
      expr = {
        GET(url = api_url, add_headers(.headers = api_headers))
      },
      error = function(e) {
        return(NULL)
      }
    )
    
    if (is.null(response)) {
      weather_data(NULL)
      error_message("Error making API call. Please check your input and try again.")
    } else {
      # Check if the API call was successful
      if (http_status(response)$category == "Success") {
        weather_data(content(response, "parsed"))
        error_message(NULL)
      } else {
        weather_data(NULL)
        error_message("Error fetching weather data. Please check your input and try again.")
      }
    }
  })
  
  output$weather_info <- renderUI({
    if (!is.null(weather_data())) {
      weather <- weather_data()
      
      tags$div(
        id = "weather_info",
        style = "border: 1px solid #ddd; padding: 10px; border-radius: 10px;",
        tags$h4("Weather Information"),
        tags$div(
          tags$p("Location: ", strong(weather$location$name, ", ", weather$location$country)),
          tags$p("Region: ", weather$location$region),
          tags$p("Latitude: ", weather$location$lat),
          tags$p("Longitude: ", weather$location$lon),
          tags$p("Local Time: ", weather$location$localtime),
          tags$p("Temperature: ", strong(weather$current$temp_c, "°C")),
          tags$p("Weather: ", weather$current$condition$text),
          tags$p("Wind Speed: ", weather$current$wind_kph, " km/h"),
          tags$p("Humidity: ", weather$current$humidity, "%"),
          tags$img(src = weather$current$condition$icon, alt = "Weather Icon", style = "max-height: 50px;"),
          tags$p("Feels Like: ", weather$current$feelslike_c, "°C"),
          tags$p("Pressure: ", weather$current$pressure_mb, " hPa")
        )
      )
    }
  })
  
  weather_forecast_data <- reactiveVal(NULL)
  observeEvent(input$get_3day_forecast_weather, {
    req(input$selected_city)

    api_url <- paste0("https://weatherapi-com.p.rapidapi.com/forecast.json?q=", input$selected_city, '&days=3')
    api_headers <- c(
      'X-RapidAPI-Key' = 'f0fee14d81msh4a61ea9554c3eebp19f257jsn98e5da8f8c90',
      'X-RapidAPI-Host' = 'weatherapi-com.p.rapidapi.com'
    )

    response <- tryCatch(
      expr = {
        GET(url = api_url, add_headers(.headers = api_headers))
      },
      error = function(e) {
        return(NULL)
      }
    )

    if (is.null(response)) {
      weather_forecast_data(NULL)
      error_message("Error making API call. Please check your input and try again.")
    } else {
      # Check if the API call was successful
      if (http_status(response)$category == "Success") {
        weather_forecast_data(content(response, "parsed"))
        error_message(NULL)
      } else {
        weather_forecast_data(NULL)
        error_message("Error fetching weather forecast data. Please check your input and try again.")
      }
    }
  })
  
  output$forecast_table <- renderTable({
    if (!is.null(weather_forecast_data())) {
      weather_forecast <- weather_forecast_data()
      forecast_data <- data.frame(
        Date = sapply(weather_forecast$forecast$forecastday, function(day) day$date),
        Max_Temperature_C = sapply(weather_forecast$forecast$forecastday, function(day) day$day$maxtemp_c),
        Avg_Temperature = sapply(weather_forecast$forecast$forecastday, function(day) day$day$avgtemp_c),
        Min_Temperature_C = sapply(weather_forecast$forecast$forecastday, function(day) day$day$mintemp_c)
      )
    }
  }, rownames = FALSE)
  
  output$temperaturePlot <- renderPlot({
    if (!is.null(weather_forecast_data())) {
      weather_forecast <- weather_forecast_data()
      
    dates <- sapply(weather_forecast$forecast$forecastday, function(day) day$date)
    max_temps <- sapply(weather_forecast$forecast$forecastday, function(day) day$day$maxtemp_c)
    avg_temps <- sapply(weather_forecast$forecast$forecastday, function(day) day$day$avgtemp_c)
    min_temps <- sapply(weather_forecast$forecast$forecastday, function(day) day$day$mintemp_c)
    
    
    #Forecast verilerini ggplot ile görselleştirme
    df <- data.frame(Date = as.Date(dates), Max_Temperature = max_temps, Min_Temperature = min_temps, Avg_Temperature = avg_temps)
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = Max_Temperature, color = "Max Temperature"), size = 1.2) +
      geom_line(aes(y = Avg_Temperature, color = "Avg Temperature"), size = 3) +
      geom_line(aes(y = Min_Temperature, color = "Min Temperature"), size = 1.2) +
      geom_label(aes(y = Max_Temperature, label = sprintf("%.1f", Max_Temperature)),
                 vjust = -0.3, hjust = 0.5, color = "black", size = 3) +
      geom_label(aes(y = Avg_Temperature, label = sprintf("%.1f", Avg_Temperature)),
                 vjust = -0.3, hjust = 0.5, color = "black", size = 3) +
      geom_label(aes(y = Min_Temperature, label = sprintf("%.1f", Min_Temperature)),
                 vjust = -0.3, hjust = 0.5, color = "black", size = 3) +
      #Dikey yazı çizgi ekleme
      geom_vline(xintercept = Sys.Date(), linetype = "dashed") + 
      #Dikey yazı ekleme
      annotate("text", x = Sys.Date(), y = (min(min_temps) + max(max_temps)) / 2, label = "bold(Today)", color = "black", size = 6, angle=90, vjust = 0,  parse = TRUE) +
      labs(title = "Weather Forecast", x = "Date", y = "Temperature (°C)") +
      scale_color_manual(values = c("Max Temperature" = "red", "Min Temperature" = "blue", "Avg Temperature" = "green")) +
      theme_gray()
    }
  })
  
  output$error_message <- renderText({
    error_message()
  })
}

shinyApp(ui, server)

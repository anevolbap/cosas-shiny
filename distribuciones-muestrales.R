##
## Teorema Central del Límite
## 
## TO DO: 

library(rmarkdown)                      
library(ggplot2)

ui <- fluidPage(
    titlePanel('Distribuciones muestrales (discretas)'),
    sidebarLayout(
        sidebarPanel(
            h4("Familia de distribuciones"),
            ## Menu de distribuciones
            selectInput("dist", label = NULL, 
                        choices = list("Bernoulli" = "bernoulli",
                                       "Binomial" = "binom",
                                       "Poisson" = "pois",
                                       "Geométrica" = "geom",
                                       "Hipergeométrica" = "hipergeom",
                                       "Binomial negativa" = "nbinom"),
                        selected = 1),
            wellPanel(
                h4("Parámetros de la distribución"),
                ## Bernoulli parameters
                conditionalPanel(
                    condition = "input.dist == 'bernoulli'",
                    sliderInput('p', 'Probabilidad de éxito: p',
                                min = 0, max = 1,
                                value = 0.5, step = 0.01,
                                animate = TRUE)
                ),
                ## Binomial parameters
                conditionalPanel(
                    condition = "input.dist == 'binom'",
                    sliderInput('nbinom', 'Número de ensayos: n',
                                min = 1, max = 100,
                                value = 1, step = 1,
                                animate = TRUE),
                    sliderInput('pbinom', 'Probabilidad de éxito: p',
                                min = 0, max = 1,
                                value = 0.5, step = 0.01,
                                animate = TRUE)
                ),
                ## Poisson parameters
                conditionalPanel(
                    condition = "input.dist == 'pois'",
                    sliderInput('lambda', withMathJax('$$\\lambda$$'),
                                min = 0, max = 100,
                                value = 10, step = 1,
                                animate = TRUE)
                ),
                ## Geometric parameters
                conditionalPanel(
                    condition = "input.dist == 'geom'",
                    sliderInput('pgeom', 'Probabilidad de éxito: p',
                                min = 0, max = 1,
                                value = 0.5, step = 0.01)
                ),
                ## Negative binomial parameters
                conditionalPanel(
                    condition = "input.dist == 'nbinom'",
                    sliderInput('rBN', 'Número de éxitos: r',
                                min = 1, max = 100,
                                value = 1, step = 1),
                    sliderInput('pBN', 'Probabilidad de éxito: p',
                                min = 0, max = 1,
                                value = 0.5, step = 0.01)
                ),
                ## Hypergeometric parameters
                conditionalPanel(
                    condition = "input.dist == 'hipergeom'",
                    sliderInput('Nhyp', 'Tamaño de la población: N', # population size
                                min = 1, max = 100,
                                value = 100, step = 1),
                    sliderInput('Khyp', 'Cantidad de elementos defectuosos: k', # success size
                                min = 0, max = 10,
                                value = 10, step = 1),
                    sliderInput('nhyp', 'Tamaño de la extracción: n', # draw 
                                min = 0, max = 20,
                                value =  10, step = 1)
                ),
                h4(" del gráfico"),
                checkboxInput('escala','Fijar la escala', value = FALSE),
                checkboxInput('esperanza','Marcar la esperanza', value = FALSE),
                conditionalPanel(
                    condition = "input.dist == 'pois'",
                    sliderInput('limpois', 'Límite derecho del gráfico',
                                min = 0, max = 50,
                                value = 5, step = 1)),
                conditionalPanel(
                    condition = "input.dist == 'geom'",
                    sliderInput('limgeom', 'Límite derecho del gráfico',
                                min = 0, max = 50,
                                value = 5, step = 1)),
                conditionalPanel(
                    condition = "input.dist == 'nbinom'",
                    sliderInput('limBN', 'Límite derecho del gráfico',
                                min = 0, max = 500,
                                value = 5, step = 1)
                ),
                conditionalPanel(
                    condition = "input.tabs == 'TCL'",
                    checkboxInput('estTCL','Estandarizar TCL',
                                  value = FALSE))),
            wellPanel(
                h4("Muestra"),
                ## Input: cantidad de observaciones
                numericInput("nobs",
                             "Tamaño de la muestra",
                             value = 50, min = 1, max = 2000, step = 10)
            )
        ),
        mainPanel(
            tabsetPanel(
                id = "tabs",
                tabPanel('Histograma',
                         h4('Función de probabilidad puntual'),
                         uiOutput('funcion_de_probabilidad_puntual'),
                         h4('Histograma'),
                         plotOutput('histograma')),
                ##          tabPanel('Acumulada',
                ##                   h4('Función de probabilidad puntual'),
                ##                   uiOutput('funcion_de_probabilidad_puntual'))
                tabPanel('Media muestral',
                         h4('Media muestral'),
                         plotOutput('mediamuestral')),
                tabPanel('LGN',
                         h4('Ley de los grandes números'),
                         plotOutput('lgn')),
                tabPanel('TCL',
                         h4('Teorema central del límite'),
                         plotOutput('tcl')),
                tabPanel('Propiedades', renderText(output$propiedades))
            )
        )
    )
)


server <- function(input, output, session) {
    ## Tab de propiedades
    output$propiedades <- reactive({includeMarkdown(paste(input$dist, 'md', sep = '.'))})

    ## Parametros de que dependen de otros parametros (hypergeom)
    observeEvent(input$Nhyp, {
        updateSliderInput(session = session, inputId = "nhyp", max = input$Nhyp)
        updateSliderInput(session = session, inputId = "Khyp", max = input$Nhyp)
    })

    ## Probabilidad puntual
    output$funcion_de_probabilidad_puntual <- renderUI({
        formula <- switch(input$dist,
                          'bernoulli' = '$$p(x;p) = \\begin{cases} 1-p &\\text{si} \\quad x=0\\\\ p &\\text{si} \\quad x=1\\end{cases}$$',
                          'binom' = '$$p(x;n,p) = \\binom{n}{x} \\cdot p^x \\cdot (1-p)^{n-x}$$',
                          'pois' = '$$p(k;\\lambda) = \\frac{e^{-\\lambda}\\lambda^k}{k!}$$',
                          'geom' = '$$p(k;p) = (1-p)^x \\cdot p $$',
                          'hyp' = '$$p(x;m,k,n) = \\frac{\\binom{K}{k}\\binom{N-K}{n-k}}{\\binom{N}{n}}$$')
        withMathJax(formula)
    })

    ## El histograma
    my_plot <- function(df, scl, esperanza) {
        g <- ggplot(data = df, aes(x = rango, y = puntual)) +
            geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
            labs(title = "", x = "", y = "") + 
            scale_x_discrete(limit = df$rango) +
            if (scl) {ylim(0, 1)}
        if (!is.nan(esperanza)) {
            g <- g + geom_vline(xintercept = esperanza,
                                linetype = "dashed", 
                                color = "orange",
                                size = 1.5)
        }
        return(g)
    }

    datos <- function() {
        
        if (input$dist == 'bernoulli') {
            soporte <- c(0, 1)
            df <- data.frame(rango = soporte, puntual = dbinom(soporte, 1, input$p))
            esperanza <- input$p
            varianza <- (1 - input$p) * input$p
            muestra <- rbinom(input$nobs, 1, input$p)
            
        }
        
        if (input$dist == 'binom') {
            soporte <- 0:input$nbinom
            df <- data.frame(rango = soporte,
                             puntual = dbinom(soporte, input$nbinom, input$pbinom))
            esperanza <- input$nbinom * input$pbinom
            varianza <- input$nbinom * (1 - input$pbinom) * input$pbinom
            muestra <- rbinom(input$nobs, input$nbinom, input$pbinom)
        }
        
        if (input$dist == 'pois') {
            soporte <- 0:input$limpois
            df <- data.frame(rango = soporte,
                             puntual = dpois(soporte, input$lambda))
            esperanza <- input$lambda
            varianza <- input$lambda
            muestra <- rpois(input$nobs, input$lambda)
        }
        
        if (input$dist == 'geom') {
            soporte <- 0:input$limgeom
            df <- data.frame(rango = soporte,
                             puntual = dgeom(soporte, input$pgeom))
            esperanza <- 1 / input$pgeom
            varianza <- (1 - input$geom) / input$geom^2
            muestra <- rgeom(input$nobs, input$pgeom)
        }
        
        if (input$dist == 'nbinom') {
            soporte <- 0:input$limBN
            df <- data.frame(rango = soporte,
                             puntual = dnbinom(soporte, input$rBN, input$pBN))
            esperanza <- input$pBN * input$rBN / (1 - input$pBN)
            varianza <- 1
            muestra <- rnbinom(input$nobs, input$rBN, input$pBN)
        }
        
        if (input$dist == 'hipergeom') {
            soporte <- 0:min(input$nhyp, input$Khyp)
            aux <- input$Nhyp - input$Khyp
            df <- data.frame(rango = soporte,
                             puntual = dhyper(soporte, input$Khyp, aux, input$nhyp))
            esperanza <- input$nhyp * input$Khyp / input$Nhyp
            varianza <- 1
            muestra <- rhyper(input$nobs, input$Khyp, aux, input$nhyp)
        }
        
        return(list(df = df,
                    esperanza = esperanza,
                    varianza = varianza,
                    muestra = muestra))
    }
        

    output$histograma <- renderPlot({
        losdatos <- datos()
        df <- losdatos$df
        esperanza <- losdatos$esperanza
        varianza <- losdatos$varianza
        muestra <- losdatos$muestra
        if (!input$esperanza) esperanza <- NaN
        my_plot(df, input$escala, esperanza)
    })
    
    output$lgn <- renderPlot({
        losdatos <- datos()
        ## lgn <- cumsum(losdatos$muestra) / (1:input$nobs)

        medias <- list()
        grilla_n <- seq(1, input$nobs, length=20)
        for (n in grilla_n) {
            muestras <- replicate (5e2, mean(datos()$muestra[1:n]))
            medias <- cbind(medias, muestras)
        }
        ylim <- extendrange(medias)
        ## Graficamos
        matplot(grilla_n, t(medias),
                pch = 16, col = "steelblue",
                main= "", lwd=2,
                ylab = "Media muestral",
                xlab = "Tamaño de muestra",
                ylim = ylim)
                ## ylim = losdatos$esperanza + sqrt(losdatos$varianza) * c(-1, 1))

        if (!input$esperanza) esperanza <- NaN
        abline(h=losdatos$esperanza, col="darkorange", lwd=2, lty=2)
        legend('topright', legend=c('Media muestral','Media poblacional'),
               lwd=c(2,2), lty=c(1,2), col=c("steelblue","orange"))
    })

    output$mediamuestral <- renderPlot({
        aux <- replicate(3e3, mean(datos()$muestra))
        hist(aux, freq = FALSE , breaks = length(datos()$muestra) / 5,
             main = paste("Histograma, n = ", length(datos()$muestra)),
             xlab = "Medias muestrales",
             col = "steelblue ", lwd = 3, density = 45,
             xlim = c(0, 1)) 
        abline(v = datos()$esperanza, col = "darkorange", lwd = 3)
    })
    
    output$tcl <- renderPlot({
        losdatos <- datos()
        aux <- replicate(3e3, {
            losdatos <- datos()
            df <- losdatos$df
            muestra <- losdatos$muestra
            esperanza <- losdatos$esperanza
            desvio <- sqrt(losdatos$varianza)
            if (input$estTCL) {
                sqrt(input$nobs) * (mean(muestra) - esperanza) / desvio
            } else {
                mean(muestra)
            }
        })

        if (input$estTCL) {
            mean_aux = 0
            sd_aux = 1
            xlab_aux = "Media muestral estandarizada"
        } else {
            mean_aux = losdatos$esperanza
            sd_aux = sqrt(losdatos$varianza / input$nobs)
            xlab_aux = "Media muestral sin estandarizar"
        }
        
        if (!input$esperanza) esperanza <- NaN
        
        xlim_aux <- extendrange(aux, f = 0.01)
        hist(aux, freq=FALSE, #breaks=input$nobs/5,
             main='Histograma del promedio',
             ylim = c(0, max(density(aux)$y) * 1.1),
             xlim = xlim_aux, density = 45, col = "gray70",
             xlab = xlab_aux)
        legend("topright", legend = c(paste("N(",losdatos$esperanza,
                                            ",", losdatos$varianza, ")",
                                            sep=""), "N(0,1)"), lty = c(2, 2), lwd = c(3, 3),
               col = c("darkorange", "steelblue"))
        curve(dnorm(x, mean = mean_aux, sd = sd_aux),
              min(aux), max(aux), add=TRUE, col="darkorange", lty=2,
              lwd=3)
        curve(dnorm(x, mean = mean_aux, sd = 1),
              min(aux), max(aux), add=TRUE, col="steelblue", lty=2,
              lwd=3)
    })
}

shinyApp(ui = ui, server = server)




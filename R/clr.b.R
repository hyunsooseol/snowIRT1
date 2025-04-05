clrClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "clrClass",
    inherit = clrBase,
    private = list(
      .htmlwidget = NULL,
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Conditional likelihood ratio tests are estimated by <b>iarm</b> R package.</li>',
            '<li>Model=RM for binary items, or model=PCM for polytomous items, is used.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        
        
        if (self$options$clr)
          self$results$clr$setNote("Note", "'Overall' indicates test of homogeneity.")
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot2$setSize(width, height)
        }
        
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
        
      },
      #########################################################
      
      .run = function() {
        data <- self$data
        groupVarName <- self$options$group
        vars <- self$options$vars
        varNames <- c(groupVarName, vars)
        model <- self$options$model
        
        if (is.null(groupVarName))
          return()
        
        data <- dplyr::select(self$data, varNames)
        
        for (var in vars)
          data[[var]] <- jmvcore::toNumeric(data[[var]])
        
        # exclude rows with missings in the grouping variable
        
        data <- data[!is.na(data[[groupVarName]]), ]
        
        #############################################################
        set.seed(1234)
        dif <- iarm::clr_tests(
          dat.items = data[, -1],
          dat.exo = data[[groupVarName]],
          model = self$options$model
        )
        #########################################################
        names <- c("Overall", groupVarName)
        clr <- as.numeric(dif[, 1])
        df <- as.numeric(dif[, 2])
        pvalue <- as.numeric(dif[, 3])
        res <- data.frame(names, clr, df, pvalue)
        
        # Creating table-------------
        if (isTRUE(self$options$clr)) {
          table <- self$results$clr
          
          lapply(1:2, function(i) {
            table$addRow(rowKey = i,
                         values = list(
                           name = res[i, 1],
                           clr = res[i, 2],
                           df = res[i, 3],
                           p = res[i, 4]
                         ))
          })
        }
        
        # Standardized residuals----------------
        
        items <- self$options$vars
        model <- self$options$model
        score <- self$options$score
        if (model == "RM") {
          set.seed(1234)
          rm.mod <- eRm::RM(X = data[, -1])
          rm <- iarm::item_obsexp(rm.mod)
          if (score == "low") {
            sc <- rm[[1]]
          }
          if (score == "high") {
            sc <- rm[[2]]
          }
          obs <- as.numeric(sc[, 1])
          exp <- as.numeric(sc[, 2])
          std <- as.numeric(sc[, 3])
          sig <- as.character(sc[, 4])
          low <- data.frame(obs, exp, std, sig)
          
          # Creating low score table-------------
          if (isTRUE(self$options$resi)) {
            table <- self$results$resi
            
            lapply(seq_along(items), function(i) {
              table$setRow(
                rowKey = items[i],
                values = list(
                  obs = low[i, 1],
                  exp = low[i, 2],
                  std = low[i, 3],
                  sig = low[i, 4]
                )
              )
            })
          }
        }
        if (model == "PCM") {
          set.seed(1234)
          pc.mod <- eRm::PCM(X = data[, -1])
          pc <- iarm::item_obsexp(pc.mod)
          if (score == "low") {
            sc <- pc[[1]]
          }
          if (score == "high") {
            sc <- pc[[2]]
          }
          obs <- as.numeric(sc[, 1])
          exp <- as.numeric(sc[, 2])
          std <- as.numeric(sc[, 3])
          sig <- as.character(sc[, 4])
          
          pc <- data.frame(obs, exp, std, sig)
          
          # Creating high score table-------------
          if (isTRUE(self$options$resi)) {
            table <- self$results$resi
            
            lapply(seq_along(items), function(i) {
              table$setRow(rowKey = items[i],
                           values = list(
                             obs = pc[i, 1],
                             exp = pc[i, 2],
                             std = pc[i, 3],
                             sig = pc[i, 4]
                           ))
            })
          }
        }
        
        # Partial Gamma to detect Differential Item Functioning (DIF)------
        if (isTRUE(self$options$dif)) {
          set.seed(1234)
          gam <- iarm::partgam_DIF(dat.items = data[, -1], dat.exo = data[[groupVarName]])
          
          gam_df <- as.data.frame(gam[c("gamma", "se", "pvalue", "lower", "upper")])
          rownames(gam_df) <- colnames(data[, -1])
          
          items <- self$options$vars
          table <- self$results$dif
          
          for (item in items) {
            row <- list(
              gamma = gam_df[item, "gamma"],
              se = gam_df[item, "se"],
              p = gam_df[item, "pvalue"],
              lower = gam_df[item, "lower"],
              upper = gam_df[item, "upper"]
            )
            table$setRow(rowKey = item, values = row)
          }
        }
        
        #  plot----------
        image <- self$results$plot
        image$setState(data[, -1])
        
        # DIF using total scores---------------
        image1 <- self$results$plot1
        state <- list(data[, -1], data[[groupVarName]])
        image1$setState(state)
        
        # DIF using class intervals---------------
        image2 <- self$results$plot2
        state <- list(data[, -1], data[[groupVarName]])
        image2$setState(state)
      },
      
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        data <- image$state
        num <- self$options$num
        plot <-  iarm::ICCplot(data = data,
                               itemnumber = num,
                               method = "score")
        print(plot)
        TRUE
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        num <- self$options$num
        
        data <- image1$state[[1]]
        group <- image1$state[[2]]
        plot1 <-  iarm::ICCplot(
          data = data,
          itemnumber = num,
          method = "score",
          icclabel = "yes",
          dif = "yes",
          difvar = group,
          difstats = "no"
        )
        print(plot1)
        TRUE
        
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        num <- self$options$num
        ci <- self$options$ci
        
        data <- image2$state[[1]]
        group <- image2$state[[2]]
        plot2 <-  iarm::ICCplot(
          data = data,
          itemnumber = num,
          method = "cut",
          cinumber = ci,
          icclabel = "yes",
          dif = "yes",
          difvar = group,
          difstats = "no"
        )
        print(plot2)
        TRUE
      }
    )
  )
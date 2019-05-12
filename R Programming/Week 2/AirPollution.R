pollutantmean <- function(directory, pollutant, id = 1:332) {
        polList <- numeric()
        for (fileID in id) {
                name <- paste(formatC(fileID, width=3, flag="0"), ".csv", sep = "")
                csvPath <- file.path(directory, name)
                csvdata <- read.csv(csvPath)
                poldata <- csvdata[[pollutant]]
                validData <- poldata[!is.na(poldata)]
                polList <- c(polList, validData)
        }
        mean <- sum(polList)/length(polList)
        mean
}

complete <- function(directory, id = 1:332) {
        df <- data.frame(id = integer(), nobs = integer())
        for (fileID in id) {
                name <- paste(formatC(fileID, width=3, flag="0"), ".csv", sep = "")
                csvPath <- file.path(directory, name)
                csvdata <- read.csv(csvPath)
                completedata <- csvdata[complete.cases(csvdata), ]
                obs <- nrow(completedata)
                obsid <- completedata$ID[1]
                df2 <- data.frame(id = obsid, nobs = obs)
                df <- rbind(df, df2)
        }
        df
}

corr <- function(directory, threshold = 0) {
        completedf <- complete(directory)
        thresdf <- completedf[which(completedf$nobs > threshold),]
        correlate <- numeric()
        for (id in thresdf$id) {
                name <- paste(formatC(id, width=3, flag="0"), ".csv", sep = "")
                csvPath <- file.path(directory, name)
                csvdata <- read.csv(csvPath)
                completedata <- csvdata[complete.cases(csvdata), ]
                relation <- cor(completedata[,2], completedata[,3])
                correlate <- c(correlate, relation)
        }
        correlate
}

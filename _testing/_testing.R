
# Example testing ---------------------------------------------------------
library(purrr)
library(TREX)
library(furrr)

plan(multiprocess)



# helper ------------------------------------------------------------------

test_summary <- function(test_list, args){

    id_force_fail_conditions <- as.vector(
        sapply(args, function(x) which(x == "fail here"),
               simplify = TRUE,
               USE.NAMES = FALSE))


    # length_forced_fail_conditions <- length(id_force_fail_conditions)



    if(length(which(is.na(test_list)))==0){
        cat("Tests passed")

    } else if(identical(id_force_fail_conditions,
                         which(is.na(test_list)) )){

        cat("Test passed with expected fails. \n Failed args_df rows =",
            which(is.na(test_list)),
            "\n")


        cat("Expected failed rows are:\n")
        print( args[which(is.na(test_list)), ])


    } else {

        cat("Failed with args_df rows =",
            which(is.na(test_list)),
            "\n")


        cat("Failed rows are:\n")
        print( args[which(is.na(test_list)), ])
    }


    return(which(is.na(test_list)))

}

# example.data ------------------------------------------------------------

input_data <- example.data(type = "timestamp")
input_data <- example.data(type = "doy")
head(input_data)

# combination of values
arg_df <- expand.grid(type = c("timestamp",
                               "doy",
                               "fail here"),
                      stringsAsFactors = FALSE)


safe_example.data <- purrr::possibly(TREX::example.data, otherwise = NA)

test_example.data <- purrr::pmap(arg_df,
                            ~safe_example.data(type = ..1)
)


test_summary(test_example.data, arg_df)





# is.trex -----------------------------------------------------------------

#validating and structuring example data
raw   <- example.data(type="doy")

# actual
input <- is.trex(raw,tz="GMT",time.format="%H:%M",
                 solar.time=TRUE,long.deg=7.7459,
                 ref.add=FALSE,df=FALSE)

head(raw)
str(input)
head(input)
plot(input)

# combination of values
arg_df <- expand.grid(solar.time = c(TRUE,FALSE),
                        long.deg = c(-500,-100, -10, 0, 10, 100,500),
                        df = c(TRUE, FALSE),
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)


# fail safely
safe_is.trex <- purrr::possibly(TREX::is.trex, otherwise = NA)


# run test
test_is.trex <- furrr::future_pmap(arg_df,
                            ~safe_is.trex(raw,
                                     tz="GMT",
                                     time.format="%H:%M",
                                    solar.time=..1,
                                    long.deg=..2,
                                    ref.add=FALSE,
                                    df=..3)
                            )


test_summary(test_is.trex, arg_df)



# additional tests


# numeric long.deg
input <- is.trex(raw,tz="GMT",time.format="%H:%M",
                 solar.time=TRUE,long.deg="7.7459",
                 ref.add=FALSE,df=FALSE)
# character solar.time
input <- is.trex(raw,tz="GMT",time.format="%H:%M",
                 solar.time="TRUE",long.deg=7.7459,
                 ref.add=FALSE,df=FALSE)



# dt.steps ------------------------------------------------------------

input <- is.trex(example.data(type="doy"),
                 tz="GMT",time.format="%H:%M", solar.time=TRUE,
                 long.deg=7.7459,ref.add=FALSE)


in.ts <- dt.steps(input=input,start='2012-06-28 00:00',end='2012-07-28 00:00',
                  time.int=60,max.gap=120,decimals=6,df=FALSE)

# manual tests
## Start date much earlier
dt.steps(input=input,start='2000-06-28 00:00',end='2012-07-28 00:00',
         time.int=60,max.gap=120,decimals=6,df=FALSE)
## fails with appropriate message



## Start date much later
dt.steps(input=input,start='2012-07-25 00:00',end='2012-07-28 00:00',
         time.int=60,max.gap=120,decimals=6,df=FALSE)
## works fine


## End date much earlier
dt.steps(input=input,start='2012-07-25 00:00',end='2000-06-28 00:00',
         time.int=60,max.gap=120,decimals=6,df=FALSE)
## fails with wrong error message


## Large time steps
dt.steps(input=input,start='2012-06-28 00:00',end='2012-07-28 00:00',
                  time.int=240,max.gap=120,decimals=6,df=FALSE)
## works fine

## Large time steps, 0 decimals
dt.steps(input=input,start='2012-06-28 00:00',end='2012-07-28 00:00',
                  time.int=240,max.gap=120,decimals=0,df=FALSE)
#3 fails appropriately

## Large time steps, character decimals
dt.steps(input=input,start='2012-06-28 00:00',end='2012-07-28 00:00',
                  time.int=240,max.gap=120,decimals="6",df=FALSE)
## fails appropriately

## Large max gap, character decimals
dt.steps(input=input,start='2012-06-28 00:00',end='2012-07-28 00:00',
         time.int=240,max.gap=600,decimals=6,df=FALSE)
## works fine


# outlier - MISSING -------------------------------------------------------





# gap.fill ----------------------------------------------------------------


# fill two hour gaps
raw   <- example.data(type = "doy")
input <-
    is.trex(
        raw,
        tz = "GMT",
        time.format = "%H:%M",
        solar.time = TRUE,
        long.deg = 7.7459,
        ref.add = FALSE,
        df = FALSE)

# create gaps in data
input[which(input < 0.4 | input > 0.82)] <- NA

fill_120 <- gap.fill(
    input = input,
    max.gap = 120,
    decimals = 10,
    df = FALSE)


fill_15 <- gap.fill(
    input = input,
    max.gap = 15,
    decimals = 10,
    df = FALSE)
# combination of values
arg_df <- expand.grid(max.gap = c(-5, 0, 10, 50, 6000),
                      decimals = c(0, 3, 5, 10),
                      df = c(TRUE, FALSE),
                      stringsAsFactors = FALSE)


safe_gap.fill <- purrr::possibly(TREX::gap.fill, otherwise = NA)

test_gap.fill <- furrr::future_pmap(arg_df,
                                 ~safe_gap.fill(input = input,
                                                max.gap = ..1,
                                                decimals = ..2,
                                                df = ..3)
)


test_summary(test_gap.fill, arg_df)

# small max.gap (10)
gap.fill(
    input = input,
    max.gap = 10,
    decimals = 5,
    df = FALSE)
# fails with inapproriate message:
    # Error in gap.fill(input = input, max.gap = 10, decimals = 5, df = FALSE) :
    # Unused argument, min.gap is smaller the minimum timestep.



# tdm_dt.max ------------------------------------------------------------------

raw <- is.trex(example.data(type = "doy"),
              tz = "GMT", time.format = "%H:%M", solar.time = TRUE,
              long.deg = 7.7459, ref.add = FALSE)
input <- dt.steps(input = raw, start = "2014-05-08 00:00",
                  end = "2014-07-25 00:50", time.int = 15, max.gap = 60,
                  decimals = 6, df = FALSE)
input[which(input<0.2)]<- NA
output.max <- tdm_dt.max(input, methods = c("pd", "mw", "dr"),
                     det.pd = TRUE, interpolate = FALSE,
                     max.days = 10, df = FALSE)

str(output.max)

plot(output.max$input, ylab = expression(Delta*italic("V")))

lines(output.max$max.pd, col = "green")
lines(output.max$max.mw, col = "blue")
lines(output.max$max.dr, col = "orange")

points(output.max$all.pd, col = "green", pch = 16)

legend("bottomright", c("raw", "max.pd", "max.mw", "max.dr"),
       lty = 1, col = c("black", "green", "blue", "orange") )




#


arg_df <- expand.grid(zero.end = c(NA, 8*60, -60),
                      zero.start = c(NA, 60, -60, 0),
                      max.days = c(NA, -5, 10),
                      # ed.window  = c(NA, -5, 2, 10),
                      df = c(TRUE, FALSE),
                      stringsAsFactors = FALSE)


safe_tdm_dt.max <- purrr::possibly(TREX::tdm_dt.max, otherwise = NA)

test_tdm_dt.max <- furrr::future_pmap(arg_df,
                                    ~safe_tdm_dt.max(input = input,
                                                   zero.end = ..1,
                                                   zero.start = ..2,
                                                   max.days = ..3,
                                                   df = ..4)
)

test_summary(test_tdm_dt.max, arg_df)




# character values for numerics
## missing informative error messages or checks for zero.start, zero.end

output.max <- tdm_dt.max(input, methods = c("pd", "mw", "dr"),
                         det.pd = TRUE, interpolate = FALSE,
                         max.days = "10", df = FALSE)


output.max <- tdm_dt.max(input, methods = c("pd", "mw", "dr"),
                         det.pd = TRUE, interpolate = FALSE,
                         zero.end = "500",
                         max.days = 10, df = FALSE)


output.max <- tdm_dt.max(input, methods = c("pd", "mw", "dr"),
                         det.pd = TRUE, interpolate = FALSE,
                         zero.end = "-100",
                         max.days = 10, df = FALSE)


output.max <- tdm_dt.max(input, methods = c("pd", "mw", "dr"),
                         det.pd = TRUE, interpolate = FALSE,
                         zero.end = NA,
                         max.days = 10, df = FALSE)



output.max <- tdm_dt.max(input, methods = c("pd", "mw", "dr"),
                         det.pd = TRUE, interpolate = FALSE,
                         zero.start = "50",
                         max.days = 10, df = FALSE)
# missing informative error messages or checks for zero.start, zero.end





# tdm_hw.cor ------------------------------------------------------------------

raw   <-is.trex(example.data(type="doy"),
                tz="GMT",time.format="%H:%M",solar.time=TRUE,
                long.deg=7.7459,ref.add=FALSE)
input <- dt.steps(input=raw,
                  start="2014-05-08 00:00",
                  end="2014-07-25 00:50",
                  time.int=15,max.gap=60,decimals=6,df=F)
input[which(input<0.2)]<-NA
input <-tdm_dt.max(input, methods=c("pd","mw","dr"),
                   det.pd=TRUE,interpolate=FALSE,max.days=10,df=FALSE)

output.data<-tdm_hw.cor(input,probe.length=20,
                        sapwood.thickness=18,df=FALSE)


plot(output.data$k.dr,col="orange")
lines(input$k.dr)




arg_df <- expand.grid(probe.length = c(NA, -5, 0, 10,20, 1000),
                      sapwood.thickness = c(NA, 10, 60, -60, 0, 100),
                      df = c(TRUE, FALSE),
                      stringsAsFactors = FALSE)


safe_tdm_hw.cor <- purrr::possibly(TREX::tdm_hw.cor, otherwise = NA)

test_tdm_hw.cor <- furrr::future_pmap(arg_df,
                                      ~safe_tdm_hw.cor(input = input,
                                                       probe.length = ..1,
                                                       sapwood.thickness = ..2,
                                                       df = ..3)
)

idx <-test_summary(test_tdm_hw.cor, arg_df)


# tests for probe lengths
output.data<-tdm_hw.cor(input,probe.length=-5,
                        sapwood.thickness=18,df=FALSE)
# negative should throw error

output.data<-tdm_hw.cor(input,probe.length=0,
                        sapwood.thickness=18,df=FALSE)
# good, but should fail with similar message as when sapwood < probe
# Warning messages:
    # 1: In tdm_hw.cor(input, probe.length = 25, sapwood.thickness = 180,  :
                         # No heartwood correction performed, the sapwood.thickness>probe.length.

output.data<-tdm_hw.cor(input,probe.length=10,
                        sapwood.thickness=18,df=FALSE)
# good


output.data<-tdm_hw.cor(input,probe.length=factor("5"),
                        sapwood.thickness=18,df=FALSE)
# good

# tests for sapwood thickness
output.data<-tdm_hw.cor(input,probe.length=25,
                        sapwood.thickness=-18,df=FALSE)
# negative should throw error

output.data<-tdm_hw.cor(input,probe.length=25,
                        sapwood.thickness=180,df=FALSE)




# tdm_damp ----------------------------------------------------------------

# Function documentation should include mention of methods (mw, dr, pd)?

raw   <-
    is.trex(
        example.data(type = "doy"),
        tz = "GMT",
        time.format = "%H:%M",
        solar.time = TRUE,
        long.deg = 7.7459,
        ref.add = FALSE
    )
input <-
    dt.steps(
        input = raw,
        time.int = 15,
        max.gap = 60,
        decimals = 6,
        df = FALSE
    )
input[which(input < 0.2)] <- NA
input <-
    tdm_dt.max(
        input,
        methods = c("pd", "mw", "dr"),
        det.pd = TRUE,
        interpolate = FALSE,
        max.days = 10,
        df = FALSE
    )
output.data <- tdm_damp(input,
                        k.threshold = 0.05,
                        make.plot = TRUE,
                        df = FALSE)
str(output.data)
head(output.data[["k.dr"]])
plot(output.data[["k.dr"]], ylab = expression(italic("K")))

## tdm_damp Gives warning message:
# Warning messages:
#     1: In `+.default`(proc.2, center) :
#     longer object length is not a multiple of shorter object length
# 2: In `+.default`(proc.2, center) :
#     longer object length is not a multiple of shorter object length
# 3: In `+.default`(proc.2, center) :
#     longer object length is not a multiple of shorter object length






arg_df <- expand.grid(k.threshold = c(NA, 0.05, 0.1, 1, -1, -0.05),
                      make.plot = c(TRUE, FALSE),
                      df = c(TRUE, FALSE))


safe_tdm_damp <- purrr::possibly(TREX::tdm_damp, otherwise = NA)

test_tdm_tdm_damp <- furrr::future_pmap(arg_df,
                                      ~safe_tdm_damp(input = input,
                                                     k.threshold = ..1,
                                                     make.plot = ..2,
                                                       df = ..3)
)


idx <-test_summary(test_tdm_tdm_damp, arg_df)


# manual

output.data <- tdm_damp(input,
                        k.threshold = -1,
                        make.plot = TRUE,
                        df = FALSE)

output.data <- tdm_damp(input,
                        k.threshold = NA,
                        make.plot = TRUE,
                        df = FALSE)


output.data <- tdm_damp(input,
                        k.threshold = 0.4,
                        make.plot = TRUE,
                        df = FALSE)
# error message is not accurate ("Unused argument k")



# tdm_cal.sfd -------------------------------------------------------------
raw   <-is.trex(example.data(type="doy"),
                tz="GMT",time.format="%H:%M",
                solar.time=TRUE,long.deg=7.7459,
                ref.add=FALSE)

input <-dt.steps(input=raw,start="2014-05-08 00:00",
                 end="2014-07-25 00:50",
                 time.int=15,max.gap=60,decimals=10,df=FALSE)

input[which(input<0.2)]<-NA

input <-tdm_dt.max(input, methods=c("pd","mw","dr"),
                   det.pd=TRUE,interpolate=FALSE,max.days=10,df=FALSE)

output.data<-tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
                         wood="Coniferous")


str(output.data)
plot(output.data$sfd.pd$sfd,ylim=c(0,10))
lines(output.data$sfd.pd$q025,lty=1,col="grey")
lines(output.data$sfd.pd$q975,lty=1,col="grey")
lines(output.data$sfd.pd$sfd)

output.data$out.param


##

output.data<-tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
                         wood="Coniferous")

arg_df <- expand.grid(wood=c("Diffuse-porous", "Ring-porous", "Coniferous"),
                      genus=c("X", "Picea", "Vitis"),
                      species = c("Picea Abies",
                                  "Vitis vinifera"),
                      a = c(NULL, NA, -1, 0,   50),
                      b = c(NULL, NA, -1, 1),
                      make.plot = c(TRUE),
                      df = c(TRUE, FALSE))


safe_tdm_cal.sfd <- purrr::possibly(TREX::tdm_cal.sfd, otherwise = NA)

test_tdm_cal.sfd <- furrr::future_pmap(arg_df,
                                        ~safe_tdm_cal.sfd(input = input,
                                                          wood = ..1,
                                                          genus = ..2,
                                                          speciies = ..3,
                                                          a = ..4,
                                                          b = ..5,
                                                          make.plot = ..6,
                                                       df = ..7)
)


idx <-test_summary(test_tdm_tdm_damp, arg_df)


tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
            wood="Diffuse-porous",
            # genus = "Vitis",
            # species = "Vitis vinifera",
            a = NA)


tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
            wood="Ring-porous",
            genus = "Vitis",
            # species = "Vitis vinifera",
            a = NA)

tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
            wood="Diffuse-porous",
            genus = "Vitis",
            # species = "Vitis vinifera",
            a = NA)

# doesn't fail  with an informative message.
# seems to be the case with other combinations also

tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
            wood="Ring-porous",
            genus = "Vitis",
            # species = "Vitis vinifera",
            a = -1)
# needs an error message

tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
            wood="Ring-porous",
            genus = "Vitis",
            # species = "Vitis vinifera",
            a = 0)
# needs an error message

tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,
            wood="Ring-porous",
            genus = "Vitis",
            # species = "Vitis vinifera",
            a = 35.90012 ,
            b = 1.194713 )
# needs an error message



# tdm_uncertain -----------------------------------------------------------
# why are arguments named differently? log.a_mu and b_mu?


raw   <- example.data(type="doy")
input <- is.trex(raw, tz="GMT", time.format="%H:%M",
                 solar.time=TRUE, long.deg=7.7459, ref.add=FALSE, df=FALSE)
input<-dt.steps(input,time.int=15,start="2013-04-01 00:00",
                end="2013-11-01 00:00",max.gap=180,decimals=15)


output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=2,sw.cor=32.28,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)

#perhaps add error message for low n values?

output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=15,sw.cor=32.28,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)
# perhaps suppress some warnings
# Warning messages:
#     1: In for (i in seq_len(n)) { ... :
#             closing unused connection 22 (<-DESKTOP-7VJ93LA:11915)
#         2: In for (i in seq_len(n)) { ... :
#                 closing unused connection 21 (<-DESKTOP-7VJ93LA:11915)
#             3: In for (i in seq_len(n)) { ... :
#                     closing unused connection 20 (<-DESKTOP-7VJ93LA:11915)
#                 4: In for (i in seq_len(n)) { ... :
#                         closing unused connection 19 (<-DESKTOP-7VJ93LA:11915)
#                     5: In for (i in seq_len(n)) { ... :
#                             closing unused connection 18 (<-DESKTOP-7VJ93LA:11915)
#                         6: In for (i in seq_len(n)) { ... :
#                                 closing unused connection 17 (<-DESKTOP-7VJ93LA:11915)


output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=50,sw.cor=32.28,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)
output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=100,sw.cor=32.28,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)
# TSI confints and values are off charts


output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=300,sw.cor=32.28,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)
# better at n = 300 (conf int still off charts, might require additional
# plot options? xlim, ylim?)

output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=300,sw.cor=15,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)

output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=300,sw.cor=NA,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)

# negative sw.cor
output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=300,sw.cor=-10,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)
# this should fail

# negative sw.cor
output<- tdm_uncertain(input, probe.length=-20, method="pd",
                       n=300,sw.cor=-10,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)
# this should fail


# agg.data ----------------------------------------------------------------
raw   <- example.data(type="doy")

input <- is.trex(raw,tz="GMT",time.format="%H:%M",
                 solar.time=TRUE,long.deg=7.7459,ref.add=FALSE,df=FALSE)

input[which(input<0.4)]<-NA

k.input<-tdm_dt.max(dt.steps(input,time.int=15,
                             max.gap=180,decimals=10),methods=c("mw"))

sfd.input<-tdm_cal.sfd(k.input,make.plot=FALSE,
                       df=FALSE,wood="Coniferous")$sfd.mw$sfd

# means
output.1hmean <- agg.data(sfd.input,
                          time.agg=60,
                          start="2012-07-28 00:00",
                          end="2012-08-29 00:00",
                          FUN="mean",
                          na.rm=TRUE,
                          df=FALSE)
output.6hmean <- agg.data(sfd.input,
                          time.agg=60*6,
                          start="2012-07-28 00:00",
                          end="2012-08-29 00:00",
                          FUN="mean",
                          na.rm=TRUE,
                          df=FALSE)
plot(output.1hmean,col="cyan")
lines(output.6hmean,col="black")

# daily sums
output.dsum<-agg.data(sfd.input,
                      time.agg=60*24,
                      start="2012-07-28 00:00",
                      end="2012-10-29 00:00",
                      FUN="sum",
                      unit=60,
                      na.rm=TRUE,
                      df=FALSE)
plot(output.dsum, type = "h")
points(output.dsum,pch=16)



agg.data(sfd.input,
         time.agg=60*24,
         start="2012-07-28 00:00",
         end="2012-10-29 00:00",
         FUN="sum",
         unit=60,
         na.rm=TRUE,
         df=FALSE)


arg_df <- expand.grid(time.agg =c(NULL, NA, -60, 0, 60, 60*12),
                      FUN = c("sum", "mean", "median", "sd", "se", "min", "max"),
                      unit = c(NULL, NA, -60, 0, 60),
                      df = c(TRUE),
                      stringsAsFactors = FALSE)


safe_agg.data <- purrr::possibly(TREX::agg.data, otherwise = NA)

test_agg.data <- furrr::future_pmap(arg_df,
                                       ~safe_agg.data(input = sfd.input,
                                                         time.agg = ..1,
                                                         start="2012-07-28 00:00",
                                                         end="2012-10-29 00:00",
                                                         FUN=..2,
                                                         unit=..3,
                                                         na.rm=TRUE,
                                                         df=..4)
)


idx <-test_summary(test_agg.data, arg_df)

arg_df[-idx,]
# Maybe should throw an error when unit is NA / negative or other?


agg.data(sfd.input,
         time.agg=60,
         start="2012-07-28 00:00",
         end="2012-10-29 00:00",
         FUN="sum",
         unit=60,
         na.rm=TRUE,
         df=FALSE)


agg.data(sfd.input,
         time.agg=60,
         start="2012-07-28 00:00",
         end="2012-10-29 00:00",
         FUN="sum",
         unit=120,
         na.rm=TRUE,
         df=FALSE)


agg.data(sfd.input,
         time.agg="60",
         start="2012-07-28 00:00",
         end="2012-10-29 00:00",
         FUN="sum",
         unit="120",
         na.rm=TRUE,
         df=FALSE)
# argument in error message does not match argument in function definition



# out.data ----------------------------------------------------------------

raw   <- is.trex(example.data(type="doy"), tz="GMT",
                 time.format="%H:%M", solar.time=TRUE,
                 long.deg=7.7459, ref.add=FALSE)

input <- dt.steps(input=raw, start="2013-05-01 00:00", end="2013-11-01 00:00",
                   time.int=15, max.gap=60, decimals=10, df=FALSE)

input[which(input<0.2)]<- NA
input <- tdm_dt.max(input, methods=c("dr"), det.pd=TRUE, interpolate=FALSE,
                max.days=10, df=FALSE)

output.data<- tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,wood="Coniferous")

input<- output.data$sfd.dr$sfd

output<- out.data(input=input, vpd.input=vpd, sr.input=sr, prec.input=preci,
                  low.sr = 150, peak.sr=300, vpd.cutoff= 0.5, prec.lim=1,
                  method="env.filt", max.quant=0.99, make.plot=TRUE)
## all data sets are in zoo, but getting warnings
## Need to change the conditional checks
# In if (zoo::index(prec.input) == FALSE) stop("Invalid input data, prec.input must be a zoo file (use is.trex).") :
#     the condition has length > 1 and only the first element will be used
#


plot(1:10)
# does not reset to original par values


# arg_df <- expand.grid(low.sr =c(-150, 150),
#                       peak.sr = c(-300, 0, 300),
#                       vpd.cutoff = c(-60, 0, 0.5),
#                       prec.lim = c(-1, 0, 1),
#                       method = c("env.filt", "stat"),
#                       max.quant = c(-1, 0,0.5, 3),
#                       make.plot = c(FALSE),
#                       stringsAsFactors = FALSE)
#
#
# safe_out.data <- purrr::possibly(TREX::out.data, otherwise = NA)
#
# test_out.data <- furrr::future_pmap(arg_df,
#                                     ~safe_out.data(input = input,
#                                                    vpd.input=vpd,
#                                                    sr.input=sr,
#                                                    prec.input=preci,
#                                                    low.sr = ..1,
#                                                    peak.sr = ..2,
#                                                    vpd.cutoff=..3,
#                                                    prec.lim=..4,
#                                                    method=..5,
#                                                    max.quant = ..6,
#                                                    make.plot = ..7)
# )


idx <-test_summary(test_out.data, arg_df)



out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         prec.input=preci,
         low.sr = -150,
         peak.sr=300,
         vpd.cutoff= 0.5,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# should throw error for low.sr


out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         # prec.input=preci,
         low.sr = 150,
         peak.sr=300,
         vpd.cutoff= 0.5,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# good


out.data(input=input,
         vpd.input=vpd,
         # sr.input=sr,
         prec.input=preci,
         low.sr = 150,
         peak.sr=300,
         vpd.cutoff= 0.5,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# check that this is compliant with desired behavior


out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         prec.input=preci,
         low.sr = -150,
         peak.sr=-300,
         vpd.cutoff= 0.5,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# should throw error for low.sr and peak.sr

out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         prec.input=preci,
         low.sr = -150,
         peak.sr=-300,
         vpd.cutoff= -0.5,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# should throw error for low.sr and peak.sr


out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         prec.input=preci,
         low.sr = -150,
         peak.sr=-300,
         vpd.cutoff= NA,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# error message not clear



out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         low.sr = -150,
         peak.sr=-300,
         vpd.cutoff= NA,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# error message wrong - should refer to prec.input


out.data(input=input,
         vpd.input=vpd,
         prec.input=preci,
         low.sr = -150,
         peak.sr=-300,
         vpd.cutoff= NA,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# error message wrong - should refer to prec.input



out.data(input=input,
         vpd.input=vpd,
         prec.input=preci,
         low.sr = -150,
         peak.sr="-300",
         vpd.cutoff= NA,
         prec.lim=1,
         method="env.filt",
         max.quant=0.99,
         make.plot=TRUE)
# good



out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         prec.input=preci,
         low.sr = -150,
         peak.sr=300,
         vpd.cutoff= 0.5,
         prec.lim=1,
         method="env.filt",
         max.quant=-0.99,
         make.plot=TRUE)
# good


out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         prec.input=preci,
         low.sr = -150,
         peak.sr=300,
         vpd.cutoff= 0.5,
         prec.lim=1,
         method="env.filt",
         max.quant=1.99,
         make.plot=TRUE)
# good


out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         prec.input=preci,
         low.sr = 150,
         peak.sr= 300,
         vpd.cutoff= 0.5,
         prec.lim=1,
         method="stat",
         max.quant=0.75,
         make.plot=TRUE)
# either suppress warnings or check if there's an alternative way to specifiy
# the condition



out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         # prec.input=preci,
         low.sr = 150,
         peak.sr= 300,
         vpd.cutoff= 0.5,
         prec.lim=1,
         method="stat",
         max.quant=0.75,
         make.plot=TRUE)
# good (but warnings as above)


out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         # prec.input=preci,
         low.sr = 150,
         peak.sr= 300,
         # vpd.cutoff= 0.5,
         prec.lim=1,
         method="stat",
         max.quant=0.75,
         make.plot=TRUE)
# good (but warnings as above)





out.data(input=input,
         vpd.input=vpd,
         sr.input=sr,
         # prec.input=preci,
         low.sr = 150,
         # peak.sr= 300,
         # vpd.cutoff= 0.5,
         # prec.lim=1,
         method="stat",
         max.quant=0.75,
         make.plot=TRUE)
# should this warning be an error?

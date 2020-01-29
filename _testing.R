
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

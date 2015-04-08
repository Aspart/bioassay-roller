# args <- list(
#   input = "/Users/roman/Projects/Biocad/bioassay-raw/2015_02_19/VAL/TR_RIT",
#   type="F",
#   ref="TR_ST",
#   test="TR_Rit",
#   aov="Day,Plate",
#   split="Plate")
#
# df = LoadAssayDataFromFolder(args$input, args$type)
# df.samples = split(df, list(df$sample, df$dose))
# lapply(lapply(df.samples, function(x) aov(response~Day*Plate, x)), summary)
#
# ggplot(.df[.df$sample==.args$sample,], aes(x=log(dose), y=response)) + geom_jitter(aes(color=Day, shape=Plate), size=4)

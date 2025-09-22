#load in FMAP data 
fmap <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/fmaps_kff.csv", skip = 2)
fmap <- fmap %>% select(-Footnotes)
# keep only the first 52 rows
fmap <- fmap[1:52, ]
fmap <- fmap %>% rename(state = Location)

View(fmap)

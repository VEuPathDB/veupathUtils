#
# you can run this in a R container...
# see ./test-container/README
#

# If testing plot.data, load plot.data environment (handle time zone, for example)
library(dotenv)
load_dot_env(file=".dev/.env")

# Load devtools (allows testing)
library(devtools)

# Load local library.
devtools::load_all("../")

# test
devtools::test("../")


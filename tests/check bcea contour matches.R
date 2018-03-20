
data(Vaccine)
m <- bcea(e=e,c=c,          # defines the variables of
          #  effectiveness and cost
          ref=2,                # selects the 2nd row of (e,c)
          interventions=treats, # defines the labels to be associated
          Kmax=50000,           # maximum value possible for the willingness
)
contour(m)

head(e) # QALY detriment
head(c) # cost

diff_e <- e[,1] - e[,2] #QALY gain
diff_c <- c[,2] - c[,1] #cost incured
diff_e %>% head()
diff_c %>% head()
res_bcea <- bcea(e = cbind(0,diff_e),
                 c = -cbind(0,diff_c))
contour(res_bcea)

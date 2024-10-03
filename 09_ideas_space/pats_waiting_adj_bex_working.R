###############################################.
### Patients Waiting - wait time adjustment ###
###############################################.

# Auhtor: Bex Madden
# Date: 3/10/2024

# Need row-wise data so start with main df
# 'stop' at end of each submission month
# avoid slices
# keep months in everything - then distinct() should be ok
# [atient will have ref date, possibly multiple app dates with assessment or dna, 
# unavailability, all before first_tret_app
# dont trim down to date range until after adjustment is done

# same principles:
# clock start date which adjusts with dnas
# unavailability for pauses
# clock stop is rolling month end NOT treatment app
Terminal command to pull data from main branch:\
`
git restore --source main -- data/
`

**Make sure to set the repo as the working directory for the app to run.**

## Current Implementations
1. User enters their condition and location.
2. Displays and ranks nearby hospitals using **synthetic data**.
3. Side panel (change location/condition, distance/number of hospitals, how the hospitals are chosen, summary statistics of the hospitals).


## Future Plans
1. Implement actual data
2. Give the user the option to select precise location in first panel (not state, not current location)
3. Ensure higher score is better
4. Customize weights for mortality and readmission measure (MORTALITY_SHARE)
5. Make the side panel toggelable
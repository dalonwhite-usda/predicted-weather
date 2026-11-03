# TODO

## Modeling Improvements

1. **Temperature reproduction function**
   - Review and implement a function describing the effects of temperature on reproduction, using the methods discussed in [the referenced paper](https://pmc.ncbi.nlm.nih.gov/articles/PMC6849717/).

2. **Temperature adult-survival function**
   - Review and implement a temperature response function for adult survival, using the same paper as a starting point.

3. **Wind-based dispersal model**
   - Develop a dispersal scale that represents the assistance wind provides to movement or spread.
   - Evaluate nonlinear response functions rather than assuming that dispersal increases linearly with wind speed.

4. **Precipitation effects on survival**
   - Model how precipitation affects survival, including the possibility of thresholds, saturation, or different effects across life stages.

5. **Host-density mapping**
   - Map host density because host availability is important for establishment and survival.

6. **Transport and connectivity**
   - Model connectivity between environments using roads, habitat suitability, and other relevant movement pathways.

## Prioritization Framework

Combine the component models into a spatial prioritization framework for short-term operational decisions. For example, A useful overall risk surface is:

$$
R_i = S_i x H_i x M_i x C_i x (1 - D_i)
$$

For each spatial unit $i$:

- $S_i$ = environmental suitability
- $H_i$ = host availability
- $M_i$ = movement or transport intensity
- $C_i$ = connectivity to downstream areas
- $D_i$ = detection and response capacity

This is an example of risk surface; inputs may vary. This is not a validated epidemiological equation. It is a practical prioritization framework. Each component should be modeled separately, evaluated, and then combined into an operational risk map.

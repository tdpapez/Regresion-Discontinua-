# Regresión Discontinua — Replicación de Ludwig y Miller (2007)

Replicación del diseño de regresión discontinua (RD) de **Ludwig y Miller (2007)**, que estima el efecto del programa Head Start sobre la mortalidad infantil, explotando el cutoff de pobreza de 1960 utilizado por la Office of Economic Opportunity (OEO) para asignar asistencia técnica a los 300 condados más pobres de Estados Unidos.

La estimación se implementa con estimadores modernos de **CCT (Calonico, Cattaneo y Titiunik)** vía el paquete `rdrobust` en R.

## Contenido

- [`TP_2_Tomas_Papez.pdf`](./TP_2_Tomas_Papez.pdf): documento completo con el análisis, resultados y apéndices (gráficos, chequeos de balance, test de densidad y placebos).

## Resumen del análisis

- **Unidad de análisis:** condado (county).
- **Running variable:** tasa de pobreza en 1960.
- **Cutoff:** 59,1984 (condado N° 300 en el ranking de pobreza).
- **Diseño:** RD Sharp, polinomio grado 1, kernel triangular, errores robustos (HC1).
- **Resultado:** efecto negativo y significativo de Head Start sobre la mortalidad infantil relacionada, interpretado como un Intention to Treat (ITT).
- **Validaciones:** continuidad en covariables predeterminadas, test de densidad, placebo de cutoff y placebos de mortalidad en grupos/causas no afectados por el programa.

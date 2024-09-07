# Tileset Generator

Este es un pequeño programa que creé hace unos años con el objetivo de facilitar la conversión de gráficos de fondo completos de ciertos juegos a un conjunto de tiles, adaptado para sistemas retro de 8 bits. El programa fue desarrollado de manera rápida y sin demasiadas pretensiones, utilizando Lazarus (Free Pascal), siendo mi primera incursión en el lenguaje Pascal. Por lo tanto, el código no es el más eficiente ni elegante, pero cumple con su propósito básico.
![Captura de pantalla 2024-09-06 230001](https://github.com/user-attachments/assets/06a7289f-31a3-4082-a9c2-84f7e3e8867f)

## Funcionalidad

El programa permite:

- Cargar un gráfico "grande" que representa el fondo completo de un juego.
- Generar un tileset basado en los bloques únicos encontrados en la imagen.
- Generar un tilemap que permite recomponer la imagen completa utilizando esos tiles.
- Exportar el tileset y el tilemap en formato binario, CSV, o a un archivo de proyecto de Tiled (`.tmx`).
  
Es importante notar que el programa **no realiza ningún tipo de procesado gráfico** (como reducción de paletas), simplemente divide la imagen en bloques y optimiza el tileset eliminando bloques repetidos.

## Estado del Proyecto

Este programa fue creado con el único propósito de realizar pruebas rápidas y, como tal, fue abandonado en una etapa muy temprana. A pesar de que es funcional, presenta varios bugs y áreas que necesitan mejoras, las cuales menciono a continuación.

### Bugs conocidos

- Problemas de precisión al generar la altura de la imagen al dibujar el tileset.
- Algunos artefactos en el gui.

### Mejoras pendientes

- Evitar la sobreescritura de archivos PNG al exportar a Tiled.
- Mejorar la estética y usabilidad de la interfaz y los menús.
- Añadir más opciones para la gestión de archivos (usando `LazFileUtils`).
- Implementar zoom para facilitar la visualización de los tiles.
- Añadir soporte para múltiples idiomas.

## Instalación y Uso

Simplemente descarga el archivo [TilesetGenerator.exe](https://github.com/marcoslm/TilesetGenerator/raw/master/TilesetGenerator.exe) y ejecútalo (Windows 7 o superior).

## Licencia

Este proyecto está licenciado bajo la **GNU General Public License v3.0 (GPL-3.0)**, lo que significa que eres libre de:

- Usar, copiar, modificar y distribuir este código con total libertad.
- Sin embargo, se requiere que cualquier modificación o distribución del código se realice bajo los mismos términos de esta licencia.

Por favor, menciona la autoría original del código cuando lo reutilices o modifiques.

Puedes consultar la licencia completa en [LICENSE](https://www.gnu.org/licenses/gpl-3.0.html).

---

**Disclaimer:** Este proyecto fue realizado como una prueba rápida para un propósito específico y no se garantiza su estabilidad o usabilidad en otros entornos.

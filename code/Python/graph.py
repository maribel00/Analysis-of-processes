# Importamos todas las librerías necesarias
import argparse
import pandas as pd
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt

parser = argparse.ArgumentParser()
parser.add_argument("archivo", help="Ruta del archivo a procesar")
args = parser.parse_args()

# Leemos el dataset
df = pd.read_csv('DatasetFusionadoCompuesto.csv')

# Eliminamos las columnas con valores NaN
df=df.dropna(axis=1)

# Ordenamos los valores según las sesiones
df.sort_values(by="Session")

# Filtrado dataset
# Años permitidos
allowed_years = [1516, 1617, 1718, 1819, 1920]

# Pedir al usuario el año
year_input = input(f"Introduzca un año (permitido: {allowed_years}, 0 para no filtrar): ")

# Comprobar si el año es válido
if year_input == "0":
    print("No se filtrará por año.")
    year_filter = int(year_input)
elif not year_input.isdigit():
    print("Error: Debe ingresar un número entero.")
elif int(year_input) not in allowed_years:
    print(f"Error: El año {year_input} no es uno de los años permitidos ({allowed_years}).")
else:
    year_filter = int(year_input)
    print(f"Se filtrará por el año {year_filter}.")

# Pedir al usuario el grupo
group_input = input("Introduzca un grupo (en formato 'DBA Año P2 NombreGrupo', 0 para no filtrar): ")

# Comprobar si el grupo es válido
if group_input == "0":
    print("No se filtrará por grupo.")
elif not group_input.startswith("DBA"):
    print("Error: Debe comenzar con 'DBA'.")
else:
    parts = group_input.split()
    if len(parts) != 4:
        print("Error: El formato debe ser 'DBA Año P2 NombreGrupo'.")
    elif not parts[1].isdigit() or int(parts[1]) not in allowed_years:
        print(f"Error: El año {parts[1]} no es uno de los años permitidos ({allowed_years}).")
    else:
        year_filter = int(parts[1])
        group_filter = parts[3]
        print(f"Se filtrará por el grupo '{group_filter}' del año {year_filter}.")

# Filtrado por año
if year_filter != 0:
    newdf = df[(df.Año == year_filter)]
if group_filter != "0":
    newdf = df[(df.Grupo == group_input)]
elif year_filter == 0:
    newdf = df

# Cálculo del número de nodos
num = newdf['Compuesto'].nunique() + 2

# Creamos una matriz de adyacencia vacía
adj_matrix = []
for i in range(num):
    new_row = []
    for j in range(num):
        new_row.append(0)
    adj_matrix.append(new_row)

# Obtenemos todos los posibles valores de la columna Compuesto y los ordenamos
labels = newdf['Compuesto'].unique().tolist()
labels.append('START')
labels.append('END')

# Crear diccionario entre labels y posiciones dentro de la lista de labels
diccionario = {}

# Iterar sobre la lista
for indice, elemento in enumerate(labels):
    # Agregar la clave-valor al diccionario
    diccionario[elemento] = indice

# Cálculo de la matriz de frecuencias
last_session = -1
last_label = 'last_label'
for index, row in newdf.iterrows():
    new_session = row['Session']
    new_label = row['Compuesto']
    if new_session == last_session:
        # Añadir una transición del último label al label actual
        adj_matrix[diccionario[last_label]][diccionario[new_label]] += 1
    else:
        if last_session != -1:
            # Añadir una transición del último label a END
            adj_matrix[diccionario[last_label]][diccionario['END']] += 1
        # Añadir una transición de START al label actual
        adj_matrix[diccionario['START']][diccionario[new_label]] += 1
    last_session = new_session
    last_label = new_label

# Pedimos al usuario el nombre del archivo donde se desean guardar los resultados
file_name = input("Introduzca el nombre del archivo donde se guardarán el label de cada nodo y la matriz de adyacencia:")

# Escribir el vector y la matriz en el mismo archivo
with open(file_name + '.txt', 'w') as f:
    label_text = ''
    for ele in labels:
        label_text = label_text + ele + ' '
    label_text = label_text + '\n'

    # Escribir el vector de las cabeceras en un archivo CSV
    f.write(label_text)

    # Escribir la matriz de adyacencia en un archivo CSV
    np.savetxt(f, adj_matrix, delimiter=' ', fmt='%d')

# Matriz vacía en la que insertaremos las diferentes aristas
edges = []

# Recorrido de la matriz original
for row in adj_matrix:
    new_row = []
    for element in row:
        if element > 0:
            new_row.append(1)
        else:
            new_row.append(0)
    edges.append(new_row)

# Crear un grafo vacío
grafo = nx.DiGraph()

# Agregar nodos al grafo
grafo.add_nodes_from(labels)

# Agregar aristas al grafo
for i in range(len(edges)):
    for j in range(len(edges)):
        if edges[i][j] == 1:
            grafo.add_edge(labels[i], labels[j], weight=adj_matrix[i][j])

# Posiciones horizontales
horizontal = []
pos_ini = 0
for i in range(len(labels)-2):
    horizontal.append(pos_ini)
    pos_ini+=2
    
# Posiciones verticales
vertical = {20:-3,40:-6,60:-9,80:-12,100:-15}


# Definir la posición de los nodos
max_problem = 0
pos = {}
for i in range(len(labels)-2):
    label = labels[i]
    r, j = label.split('-')
    r = int(r[7:])
    if r > max_problem:
        max_problem = r
    j = int(j)
    pos[label] = (horizontal[r], vertical[j])
        
pos['START'] = (max_problem,3)
pos['END'] = (max_problem,-21)
        
# Crear una figura de tamaño 10x10 pulgadas
plt.figure(figsize=(18, 24))

# Agregamos las etiquetas a los nodos
etiquetas_nodos = nx.get_node_attributes(grafo, 'label')
nx.draw_networkx_labels(grafo, pos, etiquetas_nodos, font_size=9, font_family="sans-serif")
# Agregamos las etiquetas a las aristas
edge_labels = nx.get_edge_attributes(grafo, "weight")
nx.draw_networkx_edge_labels(grafo, pos, edge_labels)

# Dibujar el grafo con posiciones fijas
nx.draw_networkx(grafo, pos=pos, node_shape='o', node_size=8000, node_color="white", edgecolors="black", linewidths=2)
# Pedir al usuario nombre del archivo donde guardar la imagen
image_name = input("Introduzca el nombre del archivo .png donde se desea guardar la imagen (sin introducir la extensión):")
# Guardar la imagen
plt.savefig(image_name + '.png')
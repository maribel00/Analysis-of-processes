import numpy as np

def removeCycles(adj_matrix):
    cycles = []
 
    # Function to mark the vertex with
    # different colors for different cycles
    def dfs_cycle(u, p, color: list, par: list):
        nonlocal cyclenumber

        # already (completely) visited vertex.
        if color[u] == 2:
            return

        # seen vertex, but was not
        # completely visited -> cycle detected.
        # backtrack based on parents to
        # find the complete cycle.
        if color[u] == 1:
            v = []
            cur = p
            v.append(cur)

            # backtrack the vertex which are
            # in the current cycle thats found
            while cur != u:
                cur = par[cur]
                v.append(cur)
            cycles.append(v)
            cyclenumber += 1

            return

        par[u] = p

        # partially visited.
        color[u] = 1

        # simple dfs on graph
        for index, v in enumerate(adj_matrix[u]):
            if v > 0:
                # if it has not been visited previously
                if index == par[u]:
                    continue
                dfs_cycle(index, u, color, par)

        # completely visited.
        color[u] = 2
        
    
    # arrays required to color the
    # graph, store the parent of node
    color = [0] * len(adj_matrix)
    par = [0] * len(adj_matrix)

    # store the numbers of cycle
    cyclenumber = 0

    # call DFS to mark the cycles
    dfs_cycle(0, len(adj_matrix), color, par)

    for cycle in cycles:
        adj_matrix[cycle[0]][cycle[-1]] = 0
        
    for i in range(len(adj_matrix)):
        for j in range(len(adj_matrix)):
            if adj_matrix[i][j] > 0 and adj_matrix[j][i] > 0:
                adj_matrix[j][i] = 0
    
    return adj_matrix

def deleteZeros(matrix, labels):
    matrix = np.array(matrix)
    labels = np.array(labels)
    
    columns = ~np.all(matrix == 0, axis=0)
    if not columns[len(matrix)-2]:
        columns[len(matrix)-2] = True
    if not columns[len(matrix)-1]:
        columns[len(matrix)-1] = True
        
    rows = ~np.all(matrix == 0, axis=1)
    
    for i in range(len(columns)):
        if not columns[i] and len(labels[i].split('-')) > 1:
            if labels[i].split('-')[1] != '20':
                for j in range(len(columns)):
                    matrix[i][j] = 0
                
    for i in range(len(matrix)-2):
        erase = True
        for j in range(len(matrix)-2):
            if matrix[i][j] > 0:
                erase = False
                break
            elif matrix[j][i] > 0:
                erase = False
                break
        if erase:
            matrix[i][len(matrix)-2] = 0;
            matrix[i][len(matrix)-1] = 0;
            matrix[len(matrix)-1][i] = 0;
            matrix[len(matrix)-2][i] = 0;
            
    columns2 = ~np.all(matrix == 0, axis=0)
    if not columns2[len(matrix)-2]:
        columns2[len(matrix)-2] = True
    if not columns2[len(matrix)-1]:
        columns2[len(matrix)-1] = True
        
    rows2 = ~np.all(matrix == 0, axis=1)

    result = [columns2[i] or rows2[i] for i in range(len(columns))]
    
    matrix_without_zero_columns = matrix[:, result]
    matrix = matrix_without_zero_columns[result, :]
    
    labels = labels[result]
    
    return matrix.tolist(), labels.tolist()
    
def myFilter(adj_matrix):
    # Transformamos la lista a un numpy array
    matrix = np.array(adj_matrix)
    matrix = matrix[:-2,:-2]
    
    # Obtenemos el número total de elementos en la matriz
    num_elem = matrix.size

    # Ordenamos los elementos de mayor a menor y los aplanamos en un array unidimensional
    sorted_elements = np.sort(matrix.flatten())[::-1]

    # Calculamos la suma acumulada de las frecuencias y la normalizamos
    if np.sum(sorted_elements) > 0:
        acumulated_frequencies = np.cumsum(sorted_elements) / np.sum(sorted_elements)

        # Encontramos el índice donde se encuentra el 90% de las frecuencias más altas
        if np.size(acumulated_frequencies) > 0:
            index = np.argmax(acumulated_frequencies >= 0.9)

            # Hacemos cero a todos los elementos que estén por debajo del corte
            matrix[matrix < sorted_elements[index]] = 0

            for i in range(matrix.shape[0]):
                for j in range(matrix.shape[1]):
                    adj_matrix[i][j] = matrix[i][j]
    
    return adj_matrix

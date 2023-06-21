import numpy as np

def removeCycles(adj_matrix):
    cycles = []
 
    # Function to mark the vertex with
    # different colors for different cycles
    def dfs_cycle(cycles, u, p, color: list, par: list):
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
                dfs_cycle(cycles, index, u, color, par)

        # completely visited.
        color[u] = 2
        
    
    # arrays required to color the
    # graph, store the parent of node
    color = [0] * len(adj_matrix)
    par = [0] * len(adj_matrix)

    # store the numbers of cycle
    cyclenumber = 0

    # call DFS to mark the cycles
    dfs_cycle(cycles, 0, len(adj_matrix), color, par)

    for cycle in cycles:
        new_cycles = []
        # arrays required to color the
        # graph, store the parent of node
        colors = [0] * len(adj_matrix)
        pars = [0] * len(adj_matrix)
        for i in range(len(adj_matrix)):
            value = adj_matrix[cycle[0]][cycle[-1]]
            adj_matrix[cycle[0]][cycle[-1]] = 0
            if i != cycle[0] and adj_matrix[i][cycle[-1]] > 0: # es un padre del nodo
                adj_matrix[i][cycle[-1]] += value
                # call DFS to mark the cycles
                dfs_cycle(new_cycles, 0, len(adj_matrix), colors, pars)
                if len(new_cycles) == len(cycles)-1:
                    break
                else:
                    adj_matrix[i][cycle[-1]] -= value
                    
    for i in range(len(adj_matrix)):
        for j in range(len(adj_matrix)):
            if adj_matrix[i][j] > 0 and adj_matrix[j][i] > 0:
                for k in range(len(adj_matrix)):
                    if adj_matrix[k][i] > 0 and k != j and adj_matrix[i][k] == 0:
                        adj_matrix[k][i] += adj_matrix[j][i]
                        break
                adj_matrix[j][i] = 0
    
    return adj_matrix

def deleteZeros(matrix, labels):
    matrix = np.array(matrix)
    labels = np.array(labels)
        
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
            
    columns = ~np.all(matrix == 0, axis=0)
    if not columns[len(matrix)-2]:
        columns[len(matrix)-2] = True

    result = [columns[i] for i in range(len(columns))]
    
    matrix_without_zero_columns = matrix[:, result]
    matrix = matrix_without_zero_columns[result, :]
    
    labels = labels[result]
    
    rows = ~np.all(matrix == 0, axis=1)
    
    for i in range(len(rows)):
        if not rows[i] and i != len(rows) - 1:
            matrix[i,len(rows)-1] = 1
    
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

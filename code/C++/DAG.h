#ifndef DAG_H
#define DAG_H

#include <string>
#include <vector>
#include <iostream>

using namespace std;

class DAG {
    public:
        vector<vector<int>> frequency;
        vector<vector<int>> duration;
        int size;
        vector<string> header;
        vector<vector<int>> paths;
        vector<vector<string>> names;
    public: // TODO: CAMBIAR
        DAG(string filename);
        vector<string> split(string id); // TODO: PASAR A PRIVATE
        int get_size(){ return size; };
        int num_problems(vector<string> path);
        vector<int> find_index(int row);
        int find_paths(int row);
        float calculate_coefficient();
        friend ostream& operator<<(ostream& ostr, const DAG& dag);
};

ostream& operator<<(ostream& ostr, const DAG& dag);

#endif // DAG_H
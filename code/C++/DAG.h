#ifndef DAG_H
#define DAG_H

#include <string>
#include <vector>
#include <iostream>

using namespace std;

class DAG {
    private:
        vector<vector<int>> frequency;
        vector<vector<int>> duration;
        bool file_correct;
        int size;
        vector<string> header;
        vector<vector<int>> paths;
        vector<vector<string>> names;

        vector<string> split(string id);
        int num_problems(vector<string> path);
        vector<int> find_index(int row);
        int find_paths(int row);
    public:
        DAG(string filename);
        vector<vector<int>> get_frequency(){ return frequency; };
        vector<vector<int>> get_duration(){ return duration; };
        bool is_correct(){ return file_correct; };
        int get_size(){ return size; };
        vector<string> get_header(){ return header; };
        vector<vector<int>> get_paths() { return paths; };
        vector<vector<string>> get_names(){ return names; };
        float get_coefficient();
        friend ostream& operator<<(ostream& ostr, const DAG& dag);
};

ostream& operator<<(ostream& ostr, const DAG& dag);

#endif // DAG_H
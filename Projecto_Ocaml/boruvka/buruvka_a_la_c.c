#include <stdio.h>
#include <stdlib.h>

typedef struct EDGE{
  int src, dest, weight;
}Edge;

typedef struct GRAPH{
  int V1, E1;
  Edge *edge[100];
}Graph;

typedef struct SUBSET{
  int parent;
  int rank;
}Subset;


int find(Subset *subsets, int i){
  if (subsets[i].parent != i){
    subsets[i].parent = find(subsets, subsets[i].parent);
  }
  return subsets[i].parent;
}


void Union(Subset *subsets, int x,int y){

  int xroot = find(subsets, x);
  int yroot = find(subsets, y);

  if (subsets[xroot].rank < subsets[yroot].rank){
    subsets[xroot].parent = yroot;
  }
  else if (subsets[xroot].rank > subsets[yroot].rank){
    subsets[yroot].parent = xroot;
  }
  else{
    subsets[yroot].parent = xroot;
    subsets[xroot].rank++;
  }
}

void Boruvka(Graph *graph){
  int V = graph->V1, E = graph->E1;
  int i, set1, set2;
  Edge *edge = *graph->edge;

  Subset *subsets;
  subsets = (Subset *) malloc (V * sizeof(Subset *));

  printf("%d\n",V );

  int *cheapest = (int *)malloc(V * sizeof(int));


  for (i = 0; i < V; ++i) {
    subsets[i].parent = i;
    subsets[i].rank = 0;
    cheapest[i] = -1;
    printf("subsets %d , %d , %d\n", subsets[i].parent ,    subsets[i].rank,cheapest[i] );
    printf("%d , %d , %d\n", graph->edge[i]->weight,  graph->edge[i]->src, graph->edge[i]->dest );
  }

  int numTrees = V;
  int MSTweight = 0;

  while(numTrees > 1){

    for(i = 0; i < E; i++){
      set1 = find(subsets, graph->edge[i]->src);
      set2 = find(subsets, graph->edge[i]->dest);
//printf("set1: %d set2: %d\n",set1,set2 );

      if (set1 == set2){
        continue;
      }else{
          if ((cheapest[set1] == -1) || (graph->edge[cheapest[set1]]->weight > graph->edge[i]->weight))
          cheapest[set1] = i;

      }
    //    printf("edge[set2]---%d----edge[i]---%d\n",graph->edge[cheapest[set1]]->weight,graph->edge[i]->weight);
      if ((cheapest[set2] == -1) || (graph->edge[cheapest[set2]]->weight > graph->edge[i]->weight)){
        cheapest[set2] = i;
      //  printf("edge[set2]---%d----edge[i]---%d\n",graph->edge[cheapest[set2]]->weight,graph->edge[i]->weight);
      }
      }


    for(i = 0; i < V; i++){

      //printf("numTrees: %d\n",graph->edge[cheapest[i]]->src);
      if (cheapest[i] != -1) {
        set1 = find(subsets, graph->edge[cheapest[i]]->src);
        set2 = find(subsets, graph->edge[cheapest[i]]->dest);

        if (set1 == set2){
          continue;
        }

        MSTweight += graph->edge[cheapest[i]]->weight;
        printf("Edge %d-%d included in Minium Spanning Tree with weight %d\n",graph->edge[cheapest[i]]->src, graph->edge[cheapest[i]]->dest, MSTweight);

        Union(subsets, set1, set2);
        numTrees--;
      }
    }


}
printf("Weight of MST is %d\n", MSTweight);
}


int main(){
  //A B C D E
  //0 1 2 3 4

int V = 5;
int E = 8;

Graph *graph = (Graph *) malloc (sizeof(Graph));
Edge *edge,*edge1,*edge2,*edge3,*edge4,*edge5,*edge6,*edge7;
edge = (Edge *) malloc (sizeof(Edge));
graph->V1 = V;
graph->E1 = E;
*graph->edge = edge;

//add edge A->B
graph->edge[0]->src = 0;
graph->edge[0]->dest = 1;
graph->edge[0]->weight = 6;
//edge = (Edge *) malloc (sizeof(Edge));
//*graph->edge = (Edge *) realloc (*graph->edge,349068348690);
//add edge A->E
edge1 = (Edge *) malloc (sizeof(Edge));
graph->edge[1]= edge1;
graph->edge[1]->src = 0;
graph->edge[1]->dest = 4;
graph->edge[1]->weight = 7;

//add edge B->C
edge2 = (Edge *) malloc (sizeof(Edge));
graph->edge[2]= edge2;
graph->edge[2]->src = 1;
graph->edge[2]->dest = 2;
graph->edge[2]->weight = 5;

//add edge B->D
edge3 = (Edge *) malloc (sizeof(Edge));
graph->edge[3]= edge3;
graph->edge[3]->src = 1;
graph->edge[3]->dest = 3;
graph->edge[3]->weight = 4;

//add edge B->E
edge4 = (Edge *) malloc (sizeof(Edge));
graph->edge[4]= edge4;
graph->edge[4]->src = 1;
graph->edge[4]->dest = 4;
graph->edge[4]->weight = 8;

//add edge C->D
edge5 = (Edge *) malloc (sizeof(Edge));
graph->edge[5]= edge5;
graph->edge[5]->src = 2;
graph->edge[5]->dest = 3;
graph->edge[5]->weight = 7;

//add edge C->E
edge6 = (Edge *) malloc (sizeof(Edge));
graph->edge[6]= edge6;
graph->edge[6]->src = 2;
graph->edge[6]->dest = 4;
graph->edge[6]->weight = 3;

//add edge D->E
edge7 = (Edge *) malloc (sizeof(Edge));
graph->edge[7]= edge7;
graph->edge[7]->src = 3;
graph->edge[7]->dest = 4;
graph->edge[7]->weight = 9;

Boruvka(graph);

return 0;
}

void * malloc(unsigned);
//char ga[20];
//void swap(char **a, char **b) {
//   char * c;
//   c = *a;
//   *a = * b;
//   *b = c;
//}
//
void * mymalloc(unsigned i) {
   return malloc(i);
}
//
//void * my_malloc(unsigned i) {
//   char * p;
//   p = mymalloc(i);
//   return p;
//}
//
//int main (){
//    char * p1, *p2;
//    char * pa, * pb;
//
//    p1 = my_malloc(10);
//    p2 = my_malloc(20);
//    swap(&p1, &p2);
//    pa = p2;
//    pb = p1;
//}

// TODO: All we compute for this is correct - we are just missing some information
// Some of the nodes in pts are dummy nodes, but if we enable them we have too much. I don't know how these are handled...
// Maybe some dummy nodes are used sometimes but not other times...


// I THINK: The nodes in ander.txt, i.e. those from the first entry (2, 3, 12, ...) and those from the GEP offsets are 
// the nodes that the analysis considers. So all others should be excluded...

#include <queue>
#include <set>
#include <unordered_map>

#include <iostream>
#include "../geometry/halfedge.h"
#include "debug.h"

/* Note on local operation return types:

    The local operations all return a std::optional<T> type. This is used so that your
    implementation can signify that it does not want to perform the operation for
    whatever reason (e.g. you don't want to allow the user to erase the last vertex).

    An optional can have two values: std::nullopt, or a value of the type it is
    parameterized on. In this way, it's similar to a pointer, but has two advantages:
    the value it holds need not be allocated elsewhere, and it provides an API that
    forces the user to check if it is null before using the value.

    In your implementaiton, if you have successfully performed the operation, you can
    simply return the required reference:

            ... collapse the edge ...
            return collapsed_vertex_ref;

    And if you wish to deny the operation, you can return the null optional:

            return std::nullopt;

    Note that the stubs below all reject their duties by returning the null optional.
*/

/*
    This method should replace the given vertex and all its neighboring
    edges and faces with a single face, returning the new face.
 */
std::optional<Halfedge_Mesh::FaceRef> Halfedge_Mesh::erase_vertex(Halfedge_Mesh::VertexRef v) {

    (void)v;
    return std::nullopt;
}

/*
    This method should erase the given edge and return an iterator to the
    merged face.
 */
std::optional<Halfedge_Mesh::FaceRef> Halfedge_Mesh::erase_edge(Halfedge_Mesh::EdgeRef e) {

    (void)e;
    return std::nullopt;
}

/*
    This method should collapse the given edge and return an iterator to
    the new vertex created by the collapse.
*/
std::optional<Halfedge_Mesh::VertexRef> Halfedge_Mesh::collapse_edge(Halfedge_Mesh::EdgeRef e) {

    (void)e;
    return std::nullopt;
}

/*
    This method should collapse the given face and return an iterator to
    the new vertex created by the collapse.
*/
std::optional<Halfedge_Mesh::VertexRef> Halfedge_Mesh::collapse_face(Halfedge_Mesh::FaceRef f) {

    (void)f;
    return std::nullopt;
}

void printHalfEdgeDetails(std::vector<Halfedge_Mesh::HalfedgeRef> h_vec) {
    for (auto &h : h_vec) {
        std::cout << "Halfedge: " << h->id() << std::endl;
        std::cout << "Vertex: " << h->vertex()->id() << std::endl;
        std::cout << "Edge: " << h->edge()->id() << std::endl;
        std::cout << "Face: " << h->face()->id() << std::endl;
        std::cout << "Next: " << h->next()->id() << std::endl;
        std::cout << "Twin: " << h->twin()->id() << std::endl;
    }
}
/*
    This method should flip the given edge and return an iterator to the
    flipped edge.
*/
std::optional<Halfedge_Mesh::EdgeRef> Halfedge_Mesh::flip_edge(Halfedge_Mesh::EdgeRef e) {

    (void)e;
    std::cout << "Flip edge " << std::endl;
    HalfedgeRef h_start = e->halfedge();
    HalfedgeRef h_prev = h_start;
    HalfedgeRef h_next = h_start->next();
    std::cout << "Starting at halfedge: " << h_start->id() << std::endl;
    std::cout << "halfedge vertex: " << h_start->vertex()->id() << std::endl;
    std::cout << "vertex halfedge : " << h_start->vertex()->halfedge()->id() << std::endl;
    std::cout << "Next halfedge: " << h_next->id() << std::endl;
    std::cout << "Next halfedge vertex: " << h_next->vertex()->id() << std::endl;
    std::cout << "Next vertex halfedge : " << h_next->vertex()->halfedge()->id() << std::endl;
    while (h_next != h_start) {
        h_prev = h_next;
        h_next = h_next->next();
        std::cout << "Next halfedge: " << h_next->id() << std::endl;
        std::cout << "Next halfedge vertex: " << h_next->vertex()->id() << std::endl;
        std::cout << "Next vertex halfedge : " << h_next->vertex()->halfedge()->id() << std::endl;
    }
    std::cout << "Prev halfedge: " << h_prev->id() << std::endl;

    HalfedgeRef h_twin_start = e->halfedge()->twin();
    HalfedgeRef h_twin_prev = h_twin_start;
    HalfedgeRef h_twin_next = h_twin_start->next();
    std::cout << "Starting at halfedge: " << h_twin_start->id() << std::endl;
    std::cout << "Next halfedge: " << h_twin_next->id() << std::endl;
    while (h_twin_next != h_twin_start) {
        h_twin_prev = h_twin_next;
        h_twin_next = h_twin_next->next();
        std::cout << "Next halfedge: " << h_twin_next->id() << std::endl;
    }
    std::cout << "Prev halfedge: " << h_twin_prev->id() << std::endl;


   // This should work for all n-gons
    // The scheme might be the edge would be re-assigned to be between the verteces one rotation away
    // from the original edge
    // IF the edge is part of a single face then it cannot be flipped
#if 0
    if (e->on_boundary()) {
        std::cout << "Boundary edge" << std::endl;
        return std::nullopt;
    }
    if (e->halfedge()->face()->is_boundary() || e->halfedge()->twin()->face()->is_boundary()) {
        std::cout << "Boundary face" << std::endl;
        return std::nullopt;
    }
#endif
#if 0
    std::vector<HalfedgeRef> halfedges;
    // Collect all halfedges
    HalfedgeRef h1 = e->halfedge();
    HalfedgeRef h1_twin = e->halfedge()->twin();
    do {
        halfedges.push_back(h1);
        h1 = h1->next();
        halfedges.push_back(h1_twin);
        h1_twin = h1_twin->next();
    } while ((h1 != e->halfedge()) || (h1_twin != e->halfedge()->twin()));
    printHalfEdgeDetails(halfedges);
    std::vector<VertexRef> vertices;
    // COllect vertices
    for (auto &h : halfedges) {
        if (std::find(vertices.begin(), vertices.end(), h->vertex()) == vertices.end()) {
            vertices.push_back(h->vertex());
        }
    }
    std::vector<EdgeRef> edges;
    // Collect edges
    for (auto &h : halfedges) {
        if (std::find(edges.begin(), edges.end(), h->edge()) == edges.end()) {
            edges.push_back(h->edge());
        }
    }

    // Collect faces
    std::vector<FaceRef> faces;
    // There are only 2 faces
    faces.push_back(halfedges[0]->face());
    faces.push_back(halfedges[1]->face());
#endif
#if 1

    // Works for all n-gons
    // Important  for an edge, the previouis haledge needs an appropriate re-assignment of the next
    // halfedge. This is because the next halfedge is the one that moves anti-clockwise
    // AFter every set_neighbor the vertex that the halfedge points to needs to be re-assigned to the halfedge. Even though it might seem obvious, it is necessary for the flip to work
    HalfedgeRef h = e->halfedge();
    FaceRef f = h->face();
    FaceRef f_twin = h->twin()->face();
    // Skipping the next halfedge.
    HalfedgeRef h_flipped = h->next();
    HalfedgeRef h_flipped_next = h_flipped->next();
    // This halfedge will change faces since it moves anti-clockwise (this is our scheme)
    VertexRef v_flipped_next = h_flipped_next->vertex();

    HalfedgeRef h_twin = h->twin();
    HalfedgeRef h_twin_flipped = h_twin->next();
    //VertexRef v_twin_flipped = h_twin_flipped->vertex();
    HalfedgeRef h_twin_flipped_next = h_twin_flipped->next();
    VertexRef v_twin_flipped_next = h_twin_flipped_next->vertex();
    // Reassign the halfedges
    h->set_neighbors(h_flipped_next, h_twin, v_twin_flipped_next, e, f);
    v_twin_flipped_next->halfedge() = h;
    h_flipped->set_neighbors(h_twin, h_flipped->twin(), h_flipped->vertex(), h_flipped->edge(), f_twin);
    h_flipped->vertex()->halfedge() = h_flipped;
    h_prev->set_neighbors(h_twin_flipped, h_prev->twin(), h_prev->vertex(), h_prev->edge(), f);
    h_prev->vertex()->halfedge() = h_prev;
    f->halfedge() = h; // This face re-assignment seems to be necessary for the flip to work

    h_twin->set_neighbors(h_twin_flipped_next, h, v_flipped_next, e, f_twin);
    v_flipped_next->halfedge() = h_twin; // This vertex re-assignment seems to be necessary for the flip to work
    h_twin_flipped->set_neighbors(h, h_twin_flipped->twin(), h_twin_flipped->vertex(), h_twin_flipped->edge(), f);
    h_twin_flipped->vertex()->halfedge() = h_twin_flipped;
    h_twin_prev->set_neighbors(h_flipped, h_twin_prev->twin(), h_twin_prev->vertex(), h_twin_prev->edge(), f_twin);
    h_twin_prev->vertex()->halfedge() = h_twin_prev;
    f_twin->halfedge() = h_twin; // This face re-assignment seems to be necessary for the flip to work


#endif
#if 0
    // Collect faces
    FaceRef f1 = e->halfedge()->face();
    std::cout << "f1: " << f1->id() << std::endl;
    FaceRef f2 = e->halfedge()->twin()->face();
    std::cout << "f2: " << f2->id() << std::endl;
    // Collect halfedges
    HalfedgeRef h1 = e->halfedge();
    HalfedgeRef h5 = e->halfedge()->twin();
    HalfedgeRef h2 = h1->next();
    HalfedgeRef h3 = h2->next();
    HalfedgeRef h4 = h3->next();
    HalfedgeRef h6 = h5->next();
    HalfedgeRef h7 = h6->next();
    HalfedgeRef h8 = h7->next();
    // Collect vertices
    VertexRef v1 = h1->vertex();
    VertexRef v2 = h5->vertex();
    VertexRef v3 = h3->vertex();
    VertexRef v4 = h4->vertex();
    VertexRef v5 = h7->vertex();
    VertexRef v6 = h8->vertex();
    // Collect edges
    EdgeRef e1 = h1->edge();
    EdgeRef e2 = h2->edge();
    EdgeRef e3 = h3->edge();
    EdgeRef e4 = h4->edge();
    //EdgeRef e5 = h5->edge();
    EdgeRef e6 = h6->edge();
    EdgeRef e7 = h7->edge();
    EdgeRef e8 = h8->edge();

    // Rotate half edge one over anti-clockwise
    // Re-assign half edges
    // void set_neighbors(HalfedgeRef next, HalfedgeRef twin, VertexRef vertex, EdgeRef edge,
    //                       FaceRef face) {
    h1->set_neighbors(h3, h5, v5, e1, f1);
    //printHalfEdgeDetails("\nh1", h1);
    h2->set_neighbors(h5, h2->twin(), v2, e2, f2);
    //printHalfEdgeDetails("\nh2", h2);
    h3->set_neighbors(h4, h3->twin(), v3, e3, f1);
    //printHalfEdgeDetails("\nh3", h3);
    h4->set_neighbors(h6, h4->twin(), v4, e4, f1);
    //printHalfEdgeDetails("\nh4", h4);
    h5->set_neighbors(h7, h1, v3, e1, f2);
    //printHalfEdgeDetails("\nh5", h5);
    h6->set_neighbors(h1, h6->twin(), v1, e6, f1);
    //printHalfEdgeDetails("\nh6", h6);
    h7->set_neighbors(h8, h7->twin(), v5, e7, f2);
    //printHalfEdgeDetails("\nh7", h7);
    h8->set_neighbors(h2, h8->twin(), v6, e8, f2);
    //printHalfEdgeDetails("\nh8", h8);

    // Re-assign faces
    f1->halfedge() = h1;
    f2->halfedge() = h5;
    // Re-assign vertices THIS IS NECESSARY
    v1->halfedge() = h6;
    v2->halfedge() = h2;
    v3->halfedge() = h3;
    v4->halfedge() = h4;
    v5->halfedge() = h7;
    v6->halfedge() = h8;
    // re-assgin edges - THIS SEEMS TO BE UNNECESSARY
    //e1->halfedge() = h1;
    //e2->halfedge() = h2;
    //e3->halfedge() = h3;
    //e4->halfedge() = h4;
    //e6->halfedge() = h6;
    //e7->halfedge() = h7;
    //e8->halfedge() = h8;

    //h1->set_next(h3);
    //h3->set_next(h7);
    // re-assign vertexes
    //h1->set_vertex(v5);
    //h5->set_vertex(v3);
    //h3->set_vertex(v6);
    //h7->set_vertex(v1);

    
    // IF the edge is part of a boundary then it cannot be flipped

    //auto h = e->halfedge();
    


    e1->halfedge() = h1;
    return e1;

#endif
#if 1
    e->halfedge() = h;
    return e;
#endif
}

/*
    This method should split the given edge and return an iterator to the
    newly inserted vertex. The halfedge of this vertex should point along
    the edge that was split, rather than the new edges.
*/
std::optional<Halfedge_Mesh::VertexRef> Halfedge_Mesh::split_edge(Halfedge_Mesh::EdgeRef e) {

    (void)e;
    return std::nullopt;
}

/* Note on the beveling process:

    Each of the bevel_vertex, bevel_edge, and bevel_face functions do not represent
    a full bevel operation. Instead, they should update the _connectivity_ of
    the mesh, _not_ the positions of newly created vertices. In fact, you should set
    the positions of new vertices to be exactly the same as wherever they "started from."

    When you click on a mesh element while in bevel mode, one of those three functions
    is called. But, because you may then adjust the distance/offset of the newly
    beveled face, we need another method of updating the positions of the new vertices.

    This is where bevel_vertex_positions, bevel_edge_positions, and
    bevel_face_positions come in: these functions are called repeatedly as you
    move your mouse, the position of which determins the normal and tangent offset
    parameters. These functions are also passed an array of the original vertex
    positions: for  bevel_vertex, it has one element, the original vertex position,
    for bevel_edge,  two for the two vertices, and for bevel_face, it has the original
    position of each vertex in halfedge order. You should use these positions, as well
    as the normal and tangent offset fields to assign positions to the new vertices.

    Finally, note that the normal and tangent offsets are not relative values - you
    should compute a particular new position from them, not a delta to apply.
*/

/*
    This method should replace the vertex v with a face, corresponding to
    a bevel operation. It should return the new face.  NOTE: This method is
    responsible for updating the *connectivity* of the mesh only---it does not
    need to update the vertex positions.  These positions will be updated in
    Halfedge_Mesh::bevel_vertex_positions (which you also have to
    implement!)
*/
std::optional<Halfedge_Mesh::FaceRef> Halfedge_Mesh::bevel_vertex(Halfedge_Mesh::VertexRef v) {

    // Reminder: You should set the positions of new vertices (v->pos) to be exactly
    // the same as wherever they "started from."
    // This is a bevelling operation for a vertex
    // The vertex is replaced by a face
    // Create a new face
    std::vector<HalfedgeRef> halfedges;
    //FaceRef f = new_face();
    HalfedgeRef h = v->halfedge(); // starting halfedge
    HalfedgeRef h_saved_start = v->halfedge(); // starting halfedge
    while (h->twin()->next() != h_saved_start) {
        halfedges.push_back(h);
        h = h->twin()->next();
    }
    

    // Create n new verteces, on each of the edges the vertex is connected to
    // Create n new edges
    // Create n new halfedges   


    (void)v;
    return std::nullopt;
}

/*
    This method should replace the edge e with a face, corresponding to a
    bevel operation. It should return the new face. NOTE: This method is
    responsible for updating the *connectivity* of the mesh only---it does not
    need to update the vertex positions.  These positions will be updated in
    Halfedge_Mesh::bevel_edge_positions (which you also have to
    implement!)
*/
std::optional<Halfedge_Mesh::FaceRef> Halfedge_Mesh::bevel_edge(Halfedge_Mesh::EdgeRef e) {

    // Reminder: You should set the positions of new vertices (v->pos) to be exactly
    // the same as wherever they "started from."

    (void)e;
    return std::nullopt;
}

/*
    This method should replace the face f with an additional, inset face
    (and ring of faces around it), corresponding to a bevel operation. It
    should return the new face.  NOTE: This method is responsible for updating
    the *connectivity* of the mesh only---it does not need to update the vertex
    positions. These positions will be updated in
    Halfedge_Mesh::bevel_face_positions (which you also have to
    implement!)
*/
std::optional<Halfedge_Mesh::FaceRef> Halfedge_Mesh::bevel_face(Halfedge_Mesh::FaceRef f) {

    // Reminder: You should set the positions of new vertices (v->pos) to be exactly
    // the same as wherever they "started from."

    (void)f;
    return std::nullopt;
}

/*
    Compute new vertex positions for the vertices of the beveled vertex.

    These vertices can be accessed via new_halfedges[i]->vertex()->pos for
    i = 1, ..., new_halfedges.size()-1.

    The basic strategy here is to loop over the list of outgoing halfedges,
    and use the original vertex position and its associated outgoing edge
    to compute a new vertex position along the outgoing edge.
*/
void Halfedge_Mesh::bevel_vertex_positions(const std::vector<Vec3>& start_positions,
                                           Halfedge_Mesh::FaceRef face, float tangent_offset) {

    std::vector<HalfedgeRef> new_halfedges;
    auto h = face->halfedge();
    do {
        new_halfedges.push_back(h);
        h = h->next();
    } while(h != face->halfedge());

    (void)new_halfedges;
    (void)start_positions;
    (void)face;
    (void)tangent_offset;
}

/*
    Compute new vertex positions for the vertices of the beveled edge.

    These vertices can be accessed via new_halfedges[i]->vertex()->pos for
    i = 1, ..., new_halfedges.size()-1.

    The basic strategy here is to loop over the list of outgoing halfedges,
    and use the preceding and next vertex position from the original mesh
    (in the orig array) to compute an offset vertex position.

    Note that there is a 1-to-1 correspondence between halfedges in
    newHalfedges and vertex positions
    in orig.  So, you can write loops of the form

    for(size_t i = 0; i < new_halfedges.size(); i++)
    {
            Vector3D pi = start_positions[i]; // get the original vertex
            position corresponding to vertex i
    }
*/
void Halfedge_Mesh::bevel_edge_positions(const std::vector<Vec3>& start_positions,
                                         Halfedge_Mesh::FaceRef face, float tangent_offset) {

    std::vector<HalfedgeRef> new_halfedges;
    auto h = face->halfedge();
    do {
        new_halfedges.push_back(h);
        h = h->next();
    } while(h != face->halfedge());

    (void)new_halfedges;
    (void)start_positions;
    (void)face;
    (void)tangent_offset;
}

/*
    Compute new vertex positions for the vertices of the beveled face.

    These vertices can be accessed via new_halfedges[i]->vertex()->pos for
    i = 1, ..., new_halfedges.size()-1.

    The basic strategy here is to loop over the list of outgoing halfedges,
    and use the preceding and next vertex position from the original mesh
    (in the start_positions array) to compute an offset vertex
    position.

    Note that there is a 1-to-1 correspondence between halfedges in
    new_halfedges and vertex positions
    in orig. So, you can write loops of the form

    for(size_t i = 0; i < new_halfedges.size(); i++)
    {
            Vec3 pi = start_positions[i]; // get the original vertex
            position corresponding to vertex i
    }
*/
void Halfedge_Mesh::bevel_face_positions(const std::vector<Vec3>& start_positions,
                                         Halfedge_Mesh::FaceRef face, float tangent_offset,
                                         float normal_offset) {

    if(flip_orientation) normal_offset = -normal_offset;
    std::vector<HalfedgeRef> new_halfedges;
    auto h = face->halfedge();
    do {
        new_halfedges.push_back(h);
        h = h->next();
    } while(h != face->halfedge());

    (void)new_halfedges;
    (void)start_positions;
    (void)face;
    (void)tangent_offset;
    (void)normal_offset;
}

/*
    Splits all non-triangular faces into triangles.
*/
void Halfedge_Mesh::triangulate() {

    // For each face...
}

/* Note on the quad subdivision process:

        Unlike the local mesh operations (like bevel or edge flip), we will perform
        subdivision by splitting *all* faces into quads "simultaneously."  Rather
        than operating directly on the halfedge data structure (which as you've
        seen is quite difficult to maintain!) we are going to do something a bit nicer:
           1. Create a raw list of vertex positions and faces (rather than a full-
              blown halfedge mesh).
           2. Build a new halfedge mesh from these lists, replacing the old one.
        Sometimes rebuilding a data structure from scratch is simpler (and even
        more efficient) than incrementally modifying the existing one.  These steps are
        detailed below.

  Step I: Compute the vertex positions for the subdivided mesh.
        Here we're going to do something a little bit strange: since we will
        have one vertex in the subdivided mesh for each vertex, edge, and face in
        the original mesh, we can nicely store the new vertex *positions* as
        attributes on vertices, edges, and faces of the original mesh. These positions
        can then be conveniently copied into the new, subdivided mesh.
        This is what you will implement in linear_subdivide_positions() and
        catmullclark_subdivide_positions().

  Steps II-IV are provided (see Halfedge_Mesh::subdivide()), but are still detailed
  here:

  Step II: Assign a unique index (starting at 0) to each vertex, edge, and
        face in the original mesh. These indices will be the indices of the
        vertices in the new (subdivided mesh).  They do not have to be assigned
        in any particular order, so long as no index is shared by more than one
        mesh element, and the total number of indices is equal to V+E+F, i.e.,
        the total number of vertices plus edges plus faces in the original mesh.
        Basically we just need a one-to-one mapping between original mesh elements
        and subdivided mesh vertices.

  Step III: Build a list of quads in the new (subdivided) mesh, as tuples of
        the element indices defined above. In other words, each new quad should be
        of the form (i,j,k,l), where i,j,k and l are four of the indices stored on
        our original mesh elements.  Note that it is essential to get the orientation
        right here: (i,j,k,l) is not the same as (l,k,j,i).  Indices of new faces
        should circulate in the same direction as old faces (think about the right-hand
        rule).

  Step IV: Pass the list of vertices and quads to a routine that clears
        the internal data for this halfedge mesh, and builds new halfedge data from
        scratch, using the two lists.
*/

/*
    Compute new vertex positions for a mesh that splits each polygon
    into quads (by inserting a vertex at the face midpoint and each
    of the edge midpoints).  The new vertex positions will be stored
    in the members Vertex::new_pos, Edge::new_pos, and
    Face::new_pos.  The values of the positions are based on
    simple linear interpolation, e.g., the edge midpoints and face
    centroids.
*/
void Halfedge_Mesh::linear_subdivide_positions() {

    // For each vertex, assign Vertex::new_pos to
    // its original position, Vertex::pos.

    // For each edge, assign the midpoint of the two original
    // positions to Edge::new_pos.

    // For each face, assign the centroid (i.e., arithmetic mean)
    // of the original vertex positions to Face::new_pos. Note
    // that in general, NOT all faces will be triangles!
}

/*
    Compute new vertex positions for a mesh that splits each polygon
    into quads (by inserting a vertex at the face midpoint and each
    of the edge midpoints).  The new vertex positions will be stored
    in the members Vertex::new_pos, Edge::new_pos, and
    Face::new_pos.  The values of the positions are based on
    the Catmull-Clark rules for subdivision.

    Note: this will only be called on meshes without boundary
*/
void Halfedge_Mesh::catmullclark_subdivide_positions() {

    // The implementation for this routine should be
    // a lot like Halfedge_Mesh:linear_subdivide_positions:(),
    // except that the calculation of the positions themsevles is
    // slightly more involved, using the Catmull-Clark subdivision
    // rules. (These rules are outlined in the Developer Manual.)

    // Faces

    // Edges

    // Vertices
}

/*
        This routine should increase the number of triangles in the mesh
        using Loop subdivision. Note: this is will only be called on triangle meshes.
*/
void Halfedge_Mesh::loop_subdivide() {

    // Compute new positions for all the vertices in the input mesh, using
    // the Loop subdivision rule, and store them in Vertex::new_pos.
    // -> At this point, we also want to mark each vertex as being a vertex of the
    //    original mesh. Use Vertex::is_new for this.
    // -> Next, compute the updated vertex positions associated with edges, and
    //    store it in Edge::new_pos.
    // -> Next, we're going to split every edge in the mesh, in any order.  For
    //    future reference, we're also going to store some information about which
    //    subdivided edges come from splitting an edge in the original mesh, and
    //    which edges are new, by setting the flat Edge::is_new. Note that in this
    //    loop, we only want to iterate over edges of the original mesh.
    //    Otherwise, we'll end up splitting edges that we just split (and the
    //    loop will never end!)
    // -> Now flip any new edge that connects an old and new vertex.
    // -> Finally, copy the new vertex positions into final Vertex::pos.

    // Each vertex and edge of the original surface can be associated with a
    // vertex in the new (subdivided) surface.
    // Therefore, our strategy for computing the subdivided vertex locations is to
    // *first* compute the new positions
    // using the connectivity of the original (coarse) mesh; navigating this mesh
    // will be much easier than navigating
    // the new subdivided (fine) mesh, which has more elements to traverse.  We
    // will then assign vertex positions in
    // the new mesh based on the values we computed for the original mesh.

    // Compute updated positions for all the vertices in the original mesh, using
    // the Loop subdivision rule.

    // Next, compute the updated vertex positions associated with edges.

    // Next, we're going to split every edge in the mesh, in any order. For
    // future reference, we're also going to store some information about which
    // subdivided edges come from splitting an edge in the original mesh, and
    // which edges are new.
    // In this loop, we only want to iterate over edges of the original
    // mesh---otherwise, we'll end up splitting edges that we just split (and
    // the loop will never end!)

    // Finally, flip any new edge that connects an old and new vertex.

    // Copy the updated vertex positions to the subdivided mesh.
}

/*
    Isotropic remeshing. Note that this function returns success in a similar
    manner to the local operations, except with only a boolean value.
    (e.g. you may want to return false if this is not a triangle mesh)
*/
bool Halfedge_Mesh::isotropic_remesh() {

    // Compute the mean edge length.
    // Repeat the four main steps for 5 or 6 iterations
    // -> Split edges much longer than the target length (being careful about
    //    how the loop is written!)
    // -> Collapse edges much shorter than the target length.  Here we need to
    //    be EXTRA careful about advancing the loop, because many edges may have
    //    been destroyed by a collapse (which ones?)
    // -> Now flip each edge if it improves vertex degree
    // -> Finally, apply some tangential smoothing to the vertex positions

    // Note: if you erase elements in a local operation, they will not be actually deleted
    // until do_erase or validate are called. This is to facilitate checking
    // for dangling references to elements that will be erased.
    // The rest of the codebase will automatically call validate() after each op,
    // but here simply calling collapse_edge() will not erase the elements.
    // You should use collapse_edge_erase() instead for the desired behavior.

    return false;
}

/* Helper type for quadric simplification */
struct Edge_Record {
    Edge_Record() {
    }
    Edge_Record(std::unordered_map<Halfedge_Mesh::VertexRef, Mat4>& vertex_quadrics,
                Halfedge_Mesh::EdgeRef e)
        : edge(e) {

        // Compute the combined quadric from the edge endpoints.
        // -> Build the 3x3 linear system whose solution minimizes the quadric error
        //    associated with these two endpoints.
        // -> Use this system to solve for the optimal position, and store it in
        //    Edge_Record::optimal.
        // -> Also store the cost associated with collapsing this edge in
        //    Edge_Record::cost.
    }
    Halfedge_Mesh::EdgeRef edge;
    Vec3 optimal;
    float cost;
};

/* Comparison operator for Edge_Records so std::set will properly order them */
bool operator<(const Edge_Record& r1, const Edge_Record& r2) {
    if(r1.cost != r2.cost) {
        return r1.cost < r2.cost;
    }
    Halfedge_Mesh::EdgeRef e1 = r1.edge;
    Halfedge_Mesh::EdgeRef e2 = r2.edge;
    return &*e1 < &*e2;
}

/** Helper type for quadric simplification
 *
 * A PQueue is a minimum-priority queue that
 * allows elements to be both inserted and removed from the
 * queue.  Together, one can easily change the priority of
 * an item by removing it, and re-inserting the same item
 * but with a different priority.  A priority queue, for
 * those who don't remember or haven't seen it before, is a
 * data structure that always keeps track of the item with
 * the smallest priority or "score," even as new elements
 * are inserted and removed.  Priority queues are often an
 * essential component of greedy algorithms, where one wants
 * to iteratively operate on the current "best" element.
 *
 * PQueue is templated on the type T of the object
 * being queued.  For this reason, T must define a comparison
 * operator of the form
 *
 *    bool operator<( const T& t1, const T& t2 )
 *
 * which returns true if and only if t1 is considered to have a
 * lower priority than t2.
 *
 * Basic use of a PQueue might look
 * something like this:
 *
 *    // initialize an empty queue
 *    PQueue<myItemType> queue;
 *
 *    // add some items (which we assume have been created
 *    // elsewhere, each of which has its priority stored as
 *    // some kind of internal member variable)
 *    queue.insert( item1 );
 *    queue.insert( item2 );
 *    queue.insert( item3 );
 *
 *    // get the highest priority item currently in the queue
 *    myItemType highestPriorityItem = queue.top();
 *
 *    // remove the highest priority item, automatically
 *    // promoting the next-highest priority item to the top
 *    queue.pop();
 *
 *    myItemType nextHighestPriorityItem = queue.top();
 *
 *    // Etc.
 *
 *    // We can also remove an item, making sure it is no
 *    // longer in the queue (note that this item may already
 *    // have been removed, if it was the 1st or 2nd-highest
 *    // priority item!)
 *    queue.remove( item2 );
 *
 */
template<class T> struct PQueue {
    void insert(const T& item) {
        queue.insert(item);
    }
    void remove(const T& item) {
        if(queue.find(item) != queue.end()) {
            queue.erase(item);
        }
    }
    const T& top(void) const {
        return *(queue.begin());
    }
    void pop(void) {
        queue.erase(queue.begin());
    }
    size_t size() {
        return queue.size();
    }

    std::set<T> queue;
};

/*
    Mesh simplification. Note that this function returns success in a similar
    manner to the local operations, except with only a boolean value.
    (e.g. you may want to return false if you can't simplify the mesh any
    further without destroying it.)
*/
bool Halfedge_Mesh::simplify() {

    std::unordered_map<VertexRef, Mat4> vertex_quadrics;
    std::unordered_map<FaceRef, Mat4> face_quadrics;
    std::unordered_map<EdgeRef, Edge_Record> edge_records;
    PQueue<Edge_Record> edge_queue;

    // Compute initial quadrics for each face by simply writing the plane equation
    // for the face in homogeneous coordinates. These quadrics should be stored
    // in face_quadrics
    // -> Compute an initial quadric for each vertex as the sum of the quadrics
    //    associated with the incident faces, storing it in vertex_quadrics
    // -> Build a priority queue of edges according to their quadric error cost,
    //    i.e., by building an Edge_Record for each edge and sticking it in the
    //    queue. You may want to use the above PQueue<Edge_Record> for this.
    // -> Until we reach the target edge budget, collapse the best edge. Remember
    //    to remove from the queue any edge that touches the collapsing edge
    //    BEFORE it gets collapsed, and add back into the queue any edge touching
    //    the collapsed vertex AFTER it's been collapsed. Also remember to assign
    //    a quadric to the collapsed vertex, and to pop the collapsed edge off the
    //    top of the queue.

    // Note: if you erase elements in a local operation, they will not be actually deleted
    // until do_erase or validate are called. This is to facilitate checking
    // for dangling references to elements that will be erased.
    // The rest of the codebase will automatically call validate() after each op,
    // but here simply calling collapse_edge() will not erase the elements.
    // You should use collapse_edge_erase() instead for the desired behavior.

    return false;
}

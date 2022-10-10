internal sealed record SequenceNode(List<Node> Nodes, Tursor Start, int Length)
: Node(Start, Length);

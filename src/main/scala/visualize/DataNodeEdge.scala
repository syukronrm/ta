package visualize

import ta.{RawEdge, RawNode}

object DataNodeEdge {
  val table_nodes = List(
    RawNode(0, 263.35423368818044, -41.66044206649194),
    RawNode(1, 238.60036976480356, 45.9301533546878),
    RawNode(2, 360.9318400625076, 21.176289431310977),
    RawNode(3, 254.29981085411475, 249.67349487786697),
    RawNode(4, 399.01470763693357, 320.1267998905551),
    RawNode(5, 391.39813412204836, 223.01548757576876),
    RawNode(6, 374.2608437135567, 122.09588850353987),
    RawNode(7, 337.6158254583112, -100.68888680685225),
    RawNode(8, 592.7007805965549, -114.48418451744215),
    RawNode(9, 430.9188510156549, -39.75629868777065),
    RawNode(10, 579.6646264266258, 0.6608449271561767),
    RawNode(11, 715.1475438849548, 87.38629950182124),
    RawNode(12, 729.757100047713, -62.241253736093114),
    RawNode(13, 851.1191130234571, 58.82759610948176),
    RawNode(14, 682.6181804926973, 229.43095243282818),
    RawNode(15, 901.739214937918, 224.74437550517135),
    RawNode(16, 787.6016375995532, 329.4186640829),
    RawNode(17, 907.1746963026433, 375.5806910625377),
    RawNode(18, 788.6284402031968, 461.49970397547213),
    RawNode(19, 657.0853235616883, 403.0615594941273),
    RawNode(20, 552.7840272538181, 322.030943269276)
  )

  val table_edges = List(
    RawEdge(1, 1, 0, None),
    RawEdge(2, 2, 0, None),
    RawEdge(3, 2, 1, None),
    RawEdge(4, 3, 1, None),
    RawEdge(5, 4, 3, None),
    RawEdge(6, 5, 3, None),
    RawEdge(7, 5, 4, None),
    RawEdge(8, 6, 2, None),
    RawEdge(9, 6, 5, None),
    RawEdge(10, 7, 0, None),
    RawEdge(11, 8, 7, None),
    RawEdge(12, 9, 7, None),
    RawEdge(13, 9, 8, None),
    RawEdge(14, 10, 6, None),
    RawEdge(15, 10, 9, None),
    RawEdge(16, 11, 10, None),
    RawEdge(17, 12, 8, None),
    RawEdge(18, 12, 10, None),
    RawEdge(19, 13, 11, None),
    RawEdge(20, 13, 12, None),
    RawEdge(21, 14, 11, None),
    RawEdge(22, 15, 13, None),
    RawEdge(23, 16, 14, None),
    RawEdge(24, 16, 15, None),
    RawEdge(25, 17, 15, None),
    RawEdge(26, 17, 16, None),
    RawEdge(27, 18, 16, None),
    RawEdge(28, 18, 17, None),
    RawEdge(29, 19, 16, None),
    RawEdge(30, 19, 18, None),
    RawEdge(31, 20, 4, None),
    RawEdge(32, 20, 14, None),
    RawEdge(33, 20, 19, None)
  )
}

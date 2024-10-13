program MazeRoutingTest;
{$RangeChecks ON}
uses MazeRouting;

var
  Map: TMap;
begin
  Clear(Map);
  Block(Map, 2, 2, 3, 3);
  Block(Map, 7, 6, 9, 9);
  Block(Map, 3, 7, 4, 8);
  Block(Map, 4, 10, 13, 14);
  Print(Map);
  Route(Map, 5, 2, 10, 15, 1);
  Print(Map);
  Route(Map, 15, 15, 1, 1, 2);
  Print(Map);
  Route(Map, 8, 3, 5, 16, 3);
  Print(Map);
  Route(Map, 13, 2, 10, 9, 4);
  Print(Map);
  Route(Map, 4, 2, 4, 16, 5);
  Print(Map);
end.

module MoveQualifier where
import Square

--
-- A qualifier is a filter that is able to decide whether
-- a square satisfies some condition.
--
-- Will be used to filter out possible source squares
-- given a hint by the user, e.g a file or a rank etc
class Qualifier src where
    qualifies :: src -> Square -> Bool


instance (Qualifier a) => Qualifier (Maybe a) where
    qualifies Nothing _ = False
    qualifies (Just x) sq = qualifies x sq

-- Return b if b qualifies as an a
qualify :: (Qualifier src) => src -> Square -> Maybe Square
qualify a b = if qualifies a b then Just b else Nothing

-- Return a if b qualifies as an a
qualifier :: (Qualifier src) => src -> Square -> Maybe src
qualifier a b = qualify a b >> Just a

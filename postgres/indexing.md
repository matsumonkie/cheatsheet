# Why

When your table has a lot of records, finding a particular one takes a lot of time as you have to parse the whole table.
To solve this issue, we can use index. Index allows you to access to specific records much faster.
RDBMS uses physical storage, and reading storage is quite slow. That's why indexing is important.

# Structure

The default postgres index structure is b-tree (balanced tree)

btree supports lot of operators (=, <, >, <>, between, like,...)

```
                root page +--+
                          |10|
                          +-++
                            |
                            |
                  page 10 +-+-+----+
                          |ca|ls|pw|   entry (ca, 1) tells us that everything in page1 is inferior to ca
                          |1 |2 |3 |
                          ++-+-++-++
                           |   |  |
                           |   |  +---------------------------------+
                           |   |                                    |
                           |   +----------------+                   |
                           |                    |                   |
 NODE LEVEL         page1 ++-+--+--+   page2 +--+----+     page3 +--+----+
                          |ak|bl|ca|         | ...   |           | ...   |
                          |4 |5 |6 |         |       |           |       |
                          +--+--+--+         +-------+           +-------+
                           |   |  |
                           |   |  +-----------------------------------------+
                           |   |                                            |
                           |   +-----------------------+                    |
                           |                           |                    |
 LEAF LEVEL         page4 +----+----+----+   page5 +---+----+     page6 +---+----+
                          |ab  |al  |ak  |-------->|ap|bb|bl|---------->|bs|bv|ca|
                          |10-1|5-37|5-12|<--------|  ....  |<----------|  ....  |
                          +----+----+----+         +--------+           +--------+


Leaf are pointing to blocks of the table (not index), so entry ab is in block 10 item number 1
Leafs are chained both ways, so you don't need to go upward once you are at the leaf level
```

With this structure we can access directly one specific value. Once we have it we can access all inferior or superior values.

create index foo on bar(a,b,c)

direct access on any criterion on a, or (a,b), or (a,b,c)
so efficient with:
  - a = c1
  - a = c1 and b = c2
  - a = c1 and b = c2 and c = c3
  or (only if the inequality is the last term)
  - a > c1
  - a = c1 and b > c2
  - a = c1 and b = c2 and c > c3
the rest work but will be slower as we can't follow the chained list

so it's redundant to have 2 indexes on (a) and (a,b). Just create one index on (a,b) works for both.

## b-trees

### where they don't work

select * from foo where date + interval '1 week' = c1
should be rewritten as:
select * from foo where date = c1 + interval '1 week'

select * from foo where my_int = 12.0;
postgres will convert every column to float like this:
select * from foo where float(my_int) = 12.0;
so it cannot search with the index

This will use the index
select * from foo where my_text_column like 'bar%'

whereas this won't
select * from foo where my_text_column like '%bar'
select * from foo where my_text_column like '%bar%'


question:
- like '%something%'


## partial indexes

when you don't want to index all records

e.g: create index foo on bar(a) where a > 12
note that the condition must be immutable for a query to use the index
so: create index foo on bar(a) where a > last_month will not work because last_month is not immutable

## covering indexes

create index tab_x_y on tab(x) include (y); put y in the index page, but don't use it.
useful if you don't want index with 'y' but still want to have fast access to it

## hash indexes

only useful for = operator

very fast
no unique constraint
insert/update concurrency not as good as b-trees
not used that much as they used to be unreliable

## GiST / SP-GiST

Generalized Search Trees (b-trees without <=> operator). Just tells you if a predicate might be true down on the node path

used for non scalar data (2 or more dimensions, geographic, geometric, ltree, full text search,...)

splitting hurt even more than on b-trees.

SP-GiST is "space partitioned", for data that can be split in totatlly distinct sets.

## GIN (Generalized Inverted Index)

forward index
docId    document
1    ->  docA
2    ->  docB
3    ->  docC
4    ->  docD

inverted index
document docId
docA     1 2 3
docB     4
docC     5
docD     6

used a lot for full text search

## BRIN (Block range indexes)

store per block range the min/max values

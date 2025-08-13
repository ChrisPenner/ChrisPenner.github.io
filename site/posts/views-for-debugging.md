---
title: "You should add debug views to your DB"
author: Chris Penner
date: Aug 13, 2025
tags: [programming, ops, databases]
description: "A nifty trick for debugging your tables easier."
image: window.jpg
---

This one will be quick.

Imagine this, you get a report from your bug tracker: 

> Sophie got an error when viewing the diff after her most recent push to her contribution to the `@unison/cloud` project on Unison Share

(BTW, contributions are like pull requests, but for Unison code)

Okay, this is great, we have something to start with, let's go look up that contribution and see if any of the data there is suspicious.

Uhhh, okay, I know the error is related to one of Sophie's contributions, but how do I actually **find it**?

I know Sophie's username from the bug report, that helps, but I don't know which project she was working on, or what the contribution ID is, which branches are involved, etc. Okay no problem, our data is relational, so I can dive in and figure it out with a query:

```sql
> SELECT 
  contribution.* 
  FROM contributions AS contribution
  JOIN projects AS project 
    ON contribution.project_id = project.id
  JOIN users AS unison_user 
    ON project.owner = unison_user.id
  JOIN users AS contribution_author 
    ON contribution.author_id = contribution_author.id
  JOIN branches AS source_branch 
    ON contribution.source_branch = source_branch.id
  WHERE contribution_author.username = 'sophie'
    AND project.name = 'cloud'
    AND unison_user.username = 'unison'
  ORDER BY source_branch.updated_at DESC

-[ RECORD 1 ]--------+----------------------------------------------------
id                   | C-4567
project_id           | P-9999
contribution_number  | 21
title                | Fix bug
description          | Prevent the app from deleting the User's hard drive
status               | open
source_branch        | B-1111
target_branch        | B-2222
created_at           | 2025-05-28 13:06:09.532103+00
updated_at           | 2025-05-28 13:54:23.954913+00
author_id            | U-1234
```

It's not the worst query I've ever had to write out, but if you're doing this a couple times a day on a couple different tables, 
writing out the joins gets pretty old **real fast**.
Especially so if you're writing it in a CLI interface where's it's a royal pain to edit the middle of a query.

Even after we get the data we get a very ID heavy view of what's going on, what's the 
actual project name? What are the branch names? Etc. 

We can solve both of these problems by writing a bunch of joins **ONCE** by creating a debugging view
over the table we're interested in. Something like this:

```sql
CREATE VIEW debug_contributions AS
SELECT 
  contribution.id AS contribution_id,
  contribution.project_id,
  contribution.contribution_number,
  contribution.title,
  contribution.description,
  contribution.status,
  contribution.source_branch as source_branch_id,
  source_branch.name AS source_branch_name,
  source_branch.updated_at AS source_branch_updated_at,
  contribution.target_branch as target_branch_id,
  target_branch.name AS target_branch_name,
  target_branch.updated_at AS target_branch_updated_at,
  contribution.created_at,
  contribution.updated_at,
  contribution.author_id,
  author.username AS author_username,
  author.display_name AS author_name,
  project.name AS project_name,
  '@'|| project_owner.username || '/' || project.name AS project_shorthand,
  project.owner AS project_owner_id,
  project_owner.username AS project_owner_username
FROM contributions AS contribution
JOIN projects AS project ON contribution.project_id = project.id
JOIN users AS author ON contribution.author_id = author.id
JOIN users AS project_owner ON project.owner = project_owner.id
JOIN branches AS source_branch ON contribution.source_branch = source_branch.id
JOIN branches AS target_branch ON contribution.target_branch = target_branch.id;
```

Okay, that's a lot to write out at once, but we never need to write that again. 
Now if we need to answer the same question we did above we do:

```sql
SELECT * from debug_contributions 
  WHERE author.username = 'sophie'
    AND project_shorthand = '@unison/cloud'
    ORDER BY source_branch_updated_at DESC;
```

Which is _considerably_ easier on both my brain and my fingers.
I also get all the information I could possibly want in the result!

You can craft one of these debug tables for whatever your needs are for each and every table you work with,
and since it's just a view, it's trivial to update or delete, and doesn't take any space in the DB itself.

Obviously querying over `project_shorthand = '@unison/cloud'` isn't going to be able to use an index, so isn't going to be the most performant query; but these are one off queries, so it's not a concern (to me at least). If you care about that sort of thing you can leave out the computed columns so you won't have to worry about that.

Anyways, that's it, that's the whole trick. Go make some debugging views and save your future self some time.

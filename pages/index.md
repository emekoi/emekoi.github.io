---
title: home
hide-title: true
---

# About Me
I am Emeka Nkurumeh, a first-year Ph.D. student at Indiana University advised by [Amr Sabry](https://homes.luddy.indiana.edu/sabry/index.html). My main interests lie in functional programming and systems programming, which means I spend most of my time thinking about compilers. Right now I'm most interested in the compilation of algebraic effect-based systems.

You can find my resume [here](/static/resume.pdf).

# Contact

::: {.dl}
Email

<{{site.email}}>

Github

[@emekoi]({{site.git}})

GPG Key

[0B81A91D60C23D68](/static/0B81A91D60C23D68.asc)
:::

{{^ site.hide-recent }}
# Recent Posts
{{> post-list.md }}
{{/ site.hide-recent }}

# Publications

::: {.dl}
{{# site.publications }}
[{{ title }}]({{ uri }})

{{ authors }}

{{/ site.publications }}
:::

``` {=raw}
<!-- # Bibliography -->
<!-- You can find a list of all the works I have cited (or hope to cite) [here](/bibliography). -->
```

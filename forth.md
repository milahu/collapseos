# [Collapse OS](readme.md) — Why Forth?

Collapse OS' first incarnation was written in Z80 assembler. One of the first feedbacks I had after it went viral was "why not Forth?". I briefly looked at it and it didn't seem such a great choice at first, so I first dismissed it. Then, I had what alcoholics refer to as a "Moment of clarity".

Forth is a stellar fit to Collapse OS design goals. If you're not familiar with it, it might be hard to understand why. Let me try to explain.

## What it's not

Some Forth enthusiasts like the language for itself. They think that its easier to express much of the logic of a program in this language. Many books and tutorials are touting this.

That's not my opinion. Forth is a fine and capable language, but I tend to agree with those who say that it tends to produce write-only programs. Most of the books on Forth are very old and compare Forth's elegance to Fortran's or COBOL's. I'm of the opinion that C is more expressive and generally produces more elegant and maintainable code.

When you look for Forth's advantages, don't look there.

Forth isn't particularly fast either. As an interpreted language, it is blazing fast, but the speed tax of the threaded model still has to be paid, so it will generally be slower than code produced by a C compiler.

However, as a Python developer, I know this tradeoff well. It's not 100% of your program that needs to be fast, only bottlenecks. Like with Python, it's easy in Forth to write bottlenecks in native code. Therefore, speed is not such a big issue.

## What it is

Forth is, to my knowledge, the most *compact* language allowing high level constructs. It is so compact that Collapse OS' Forth implementation achieves self-hosting with about the same amount of resources than its assembler counterpart!

*What is this sorcery?* At face value, nothing can be more compact than native code. It's self-evident. If a piece of Forth code is doing the exact same piece of logic in a way that is as compact as native code, you've been lazy on your assembly writing!

To understand this compactness, we have to look deeper.

The Z80 asm version of Collapse OS self-hosts on a RC2014 with a 5K shell on ROM, a 5K assembler binary loaded in RAM from SD card (but that could be in ROM, that's why I count it as ROM in my project's feature highlights) and 8K of RAM. That is, it can assemble itself from source within those resources.

The biggest piece of software to assemble is the assembler itself. It's a reasonably well-featured assembler that supports forward labels, includes, many useful directives. The code implementing those features requires those aforementioned resources to assemble.

...But also, the features included in the assembler are required to assemble its source code! I could cut a bit on its features to reduce resource usage, but I'd need to adapt the assembler's code for this lack of features and possibly make the code much more complex. I believe that with this assembler, I stroke a good balance and achieved something close to optimal.

If I wanted to re-implement that assembler feature-for-feature in Forth, it would probably require much more resources to build. Even though higher level words are more compact, the base of the pyramid to get there couldn't compete with the straight assembler version. This was under this reasoning that I first dismissed Forth.

So, again, what makes Forth more compact than assembler? *Simplicity.* The particularity of Forth is that it begins "walking by itself", that is, implementing its own words from its base set, very, *very* early. This means that only a tiny part of it needs to be assembled into native code. This tiny part of native code requires much less tooling, and thus an assembler with much less features. This assembler requires less RAM.

What is more compact than something that doesn't exist? Even Z80 assembler can't beat the void.

That's how although Forth is not more compact than native code (duh!), *a Forth Collapse OS achieves self-hosting with as much resources than its Z80 counterpart*.

So for the very low price of manageable speed constraints, we get a high level language that is much more powerful and extensible than the BASIC shell that was previously there. I call that a definite win.

## Cherry on top

Because compactness has always been a primary design goal of Collapse OS, I initially chose Z80 assembler over high level languages so that I could continue to meet that design goal.

However, now that I can have my cake and eat it too, thanks to Forth, new opportunities open. First and foremost, multiple architectures! Although I was ready to give that up when I tied myself to the Z80, I'm now very glad to have it and will certainly take advantage of it.

*Note: This text was written shortly after Collapse OS' Forth rewrite, before it started supporting multiple architectures. This is why this "cherry on top" is evoked as something to reach for. Collapse OS has been running on multiple CPUs for a while now.*

## Culture of simplicity

Forth's culture of simplicity is worth writing about. At face value, a traditional Forth environment is severly constrained according to modern standards.

Who would want to work within the constraint of 64x16 blocks filesystem? Someone who knows the complexity associated with implementing a filesystem and everything that underpins it. When simplicity is valued, such constraints are gladly accepted.

We can't say that asceticism is a dominant trait of our relation to technology. We aren't used to willingly accept constraints with regards to what we could call our convenience. Why would anyone not want a piece of software or hardware to be easier to use, more accessible to the layman, more featureful? If someone is willing to build that complexity, Moore's law allow us to adopt it, and we do. This leads to cancerous complexity, layers upon layers upon layers with each new generation of developers blissfully accepting the previous layers.

(Something analogous can probably be said about our collapsing civilization, and probably even eloquently quote Joseph Tainter)

The base design of Forth is entirely built around a *primal simplicity*. Even better, the path of least resistance generally guides you towards simplicity. When a word definition becomes too big and complicated, it generally prompts you to reorganize your code toward a simpler solution that might not have occurred to you in another language.

To be clear: Forth doesn't elegantly describe complex algorithms, at least not when compared to languages like C. Those generally make the head hurt when reading. However, this mental pain does makes you question your need for complexity and encourages "sideways" solutions.

This is something that I think few developers are familiar with and is hard to describe with words. It needs to be experienced. One of the good ways to experience it I think is to implement your own Forth.

## Addendum: Forth's readability

When I wrote the text above over a year ago, I had successfully implemented my own Z80 Forth, but I couldn't consider myself a competent Forth programmer yet. Now I think that I can and that I've done enough Forth to speak about readability in a bit more details.

Forth is hard to read. Maybe you can create abstractions to make it easier to read, but it's going to cost you in terms of simplicity and at some point it becomes self-defeating: maybe the language you're after isn't Forth.

I think it's inherent to Forth, reading it simply requires a much higher cognitive load because you have to hold a lot more information into your head as you read along. It's exhausting for the mind (but you get better at it after a while). It is also slower. Accept it or you're going to have a bad time.

This doesn't mean, however, that working within an existing Forth codebase is significantly harder than with other languages: simplicity means you have a lot less code to manage. At least it doesn't *have* to be (any programmer can create an unworkable mess in any language, you know that).

What I think is key to keep a codebase workable is to, at all times, favor simplicity. When the meaning of a word is obvious, you don't need to read its content, so who cares if its code is "write only"? Rewrite it if you're unhappy with it, it's most likely a one-liner anyway.

With experience, I find that I rarely read my past code. I spend more time making a mental inventory of words I'm going to use for the words I'm about to write. This is different from reading code.

When I do read code, I admit that it's difficult, but most of the time, I only read it to make sure that I understand what the word was doing before I re-write it entirely. With experience, I end up relying a lot more on my own documentation (word listings with signatures and descriptions) instead of relying on re-reading my code, as I have the habit of doing when working in other languages.

Reading Forth is slower, but writing it doesn't have to be. However, if your mind is all excited to reach the solution you're thinking about and you're writing it rapidly, it usually means words with fuzzy meanings, which is hard to maintain. I have to quote Yoda here.

> How do I know I'm writing good Forth code, Virgil? When you are calm, at peace, passive.

I'm throwing an open question into the wind: Maybe readability is overrated?

One thing I can confirm, however, is that Forth's path of least resistance is simplicity and this is *awesome*. It requires more discipline, some brain training to handle cognitive load, but the more I do it, the more I like it.

## Addendum 2: Tumble Forth

My love letter to Forth continues in my [Tumble Forth blog](http://tumbleforth.hardcoded.net).

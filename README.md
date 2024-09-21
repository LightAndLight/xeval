# `xeval`

Evaluate arithmetic in text selections.

TODO: videos here

## Motivation

I came up with the idea by allowing myself to dream bigger about what a computing system could do.
You can read more about that thought process on my blog: [Unfettering the imagination - blog.ielliott.io](https://blog.ielliott.io/unfettering-the-imagination).

Concretely this ended up being an investigation into the [malleability](https://malleable.systems/) of the X window manager.
My system comes pre-configured with some text selection verbs: copy, paste.
I wanted to know whether I could teach my system a new one: compute-and-replace.

I say "teach the *system*" because I shouldn't have to modify any of my installed GUI programs in order to use this new feature.
I would be simultaneously extending every GUI program I've installed, plus all the those I *could* install in the future.
This is empowering, because I'm not dependent on each program's author to provide me the feature,
and it's economical, because the code I write would work in more contexts than I could have accounted for.

## Limitations

* `xeval` presses `Ctrl+V` on the target window to paste.
  If your program uses a different key combination for paste, then `xeval` won't work as intended.

## Known issues

### Firefox

* Doesn't work in the URL bar

  Pasting into URL bar sends 2 selection requests.
  The response to the first is ignored, response to the second updates the bar.

* Ctrl+A doesn't update primary selection

  If you select something with Ctrl+A then it will be ignored.
  Whatever was previously selected on the system will be used instead.

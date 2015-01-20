# Replayed

What it is now :

- a tool for managing MarkDown documents
- a pet project written in my free time to get the feel of browser dev
  and [Scala.js](http://www.scala-js.org)
- **Prototype beta version**, slightly used with lots of unit tests,
  not optimized for performance and reliability yet (you've been warned)

What I want it to be :

- a specification / documentation / project management tool,
  designed by programmer for programmers
- no bullshit like scrum, agile or gantt, just a pragmatical way of getting
  shit done and allowing for seamless cooperation of a small team on a project

Is it going to? :

- I'm gonna cool it down for a while to think about whether to continue
  or just support what it is now, because :
    - I'd like to do something that actually pays my bills now
    - I came from incredibly friendly server side to a browser environment
      that surprised me by its hostility
    - I expected the cutting edge browser specs and DOM to be more stable
      and supported than they actually are now
    - I stopped keeping it cross-browser compatible at the end because
      it consumed most of my dev time (I'm not sure if it is even possible)
    - due to my utter lack of CSS exp. (hello bootstrap) I'm constantly causing
      breaking changes in the UI (it feels like using weakly typed language)

It is composed of :

- custom impl. of full-fledged HTML textarea on the left - expecting markdown input
> which is basically an alternative to using Ace Editor or CodeMirror
> however there is no feature known to me that I wouldn't implement.
> There are additional capabilities though I've done for convenience.
- markdown processor on the right side - rendering html output of MD processing
- navigation bar at the top - allowing for document management

It was designed for :

- easy project collaboration on a project among team members
- peer-2-peer communication, having no server, relying on WebRTC only
- verifiability and versioning - the entire history of each key stroke is captured
  and available, it is quite similar to event sourcing and Akka persistence
> **Note** that any features regarding collaboration, webRTC, PM or writing specs
> are not yet implemented. What you see is just a prototype
> that allows for writing markdown documents

It was built using [Scala.js](http://www.scala-js.org) :

- there is zero lines of handwritten Javacsript used, not even as a dependency
- every part of this app is written in scala, compiled to JS + some HTML & CSS

Limitations :

- due to the nature of document data structure and persistece (sequence of key
  strokes instead of a string blob) there is a chance that rendering document
  might yield unexpected results due to some nuances in user's environment
  (Browser environment is just not as steady as server one that I am used to)
- I don't have resources to test it in all browsers
  currently **only Chrome is supported (developed on Chrome 38)**
- **mobile devices are not supported** and certainly won't be any time soon
> As you can imagine, cross-browser compatibility is a serious issue here

Development :

- it has not been published yet since it depends on an unaccepted PR and a fork
- which means that building it would require installing my fork of uTest and Actuarius

### Version

- Replayed 0.1
- Scala.js 6.0.0

### Technologies

- [Reactive IDB](https://github.com/viagraphs/scalajs-rx-idb)
  Rx based wrapper around IndexedDB I wrote
- [Monifu](http://www.monifu.org)
  Reactive Programming for Scala and Scala.js
- [ScalaTags](https://github.com/lihaoyi/scalatags)
  small XML/HTML construction library for Scala
- [Actuarius](https://github.com/l15k4/actuarius/tree/scalajs)
  markdown processor for the JVM written in Scala.
- [uPickle](https://github.com/lihaoyi/upickle)
  lightweight serialization library for Scala
- [uTest](https://github.com/lihaoyi/utest)
  tiny, portable testing library for Scala
- [bootstrap](http://getbootstrap.com)
  actually something I shouldn't have used as a CSS newbie

### License

The MIT License (MIT)
Copyright (c) 2015 by [viagraphs](https://github.com/viagraphs)
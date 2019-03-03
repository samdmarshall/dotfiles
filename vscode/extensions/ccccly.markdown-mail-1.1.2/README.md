# Markdown Mail README

**Markdown Mail! Make mail Great Again!** Just write with Markdown and
type "**ctrl+shift+p**" (Mac: '**cmd+shift+p**'), which brings up the 
**Command Palette**. From here, you type "**send**" and select "**Send My Email**"
and you are good to go. A few seconds later, your email should arrive destination.

First of all. **Config** your email accout and email setting! [See Here](#Settings)

If you see this at **github**, you can build&install this extension according to [vsc-extension-quickstart](https://github.com/Excited-ccccly/markdown-mail/blob/master/vsc-extension-quickstart.md#Packaging)

Or you can simply search and download **markdown-mail** at vscode extension management panel according to this [manual](http://code.visualstudio.com/docs/extensions/install-extension)

## Features

### Open Source!

See at [Github](https://github.com/Excited-ccccly/markdown-mail)

### WordCount
  More accurate! Support Chinese!

![WordCount](https://github.com/Excited-ccccly/markdown-mail/raw/master/resources/word-count.png)

### Markdown and Send!
  Write your email leveraging the power of markdown and send the email to your friend.

![send](https://github.com/Excited-ccccly/markdown-mail/raw/master/resources/send.png)

## Requirements

* You should have **a email account of which stmp service is avaliable**.

  Currently, this extension support **outlook**, **hotmail**, **gmail**, **qq**, **163**, **126**
  and most importantly, **ecnu**( email service provided by my school🙇 )

* If your service (ex: gmail) uses **two-step authentication**, use an application specific password

<h2 id="Settings">Extension Settings</h2>

Open your workspace setting in '**./vscode/settings.json**' or vscode's user setting globally

![Extension Config](https://github.com/Excited-ccccly/markdown-mail/raw/master/resources/extension-config.png)
Add Configuration shown in figure above
  * `markdown-mail.account`: Config your email account. Such as:

  ```
  {
    "user": "username@your-email.com",
    "password": "your-password"
  }
  ```

  * `markdown-mail.email`: Everytime your want send a email, config your email first!
  Sorry for this inconvenience, but I have to know whom you want to send to, what subject, and maybe cc. 😀

  ```
  {
    "to": "username@destination.com",
    "subject": "test markdown-mail",
    "cc": "cc@destination" // optional
  }
  ```
  
## Known Issues

Sent Email may be regarded as junk mail and be rejected.

Code Block won't be highlighted if the email service provider doesn't support highlight.js.

## Contributing

Everything is welcome.

## Release Notes

### 1.0.0

Initial release. Basicly function but enough

----------------------------------------------

**Enjoy!**
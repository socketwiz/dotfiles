'use strict'

const EventEmitter = require('zm-event-kit').Emitter
class Communication extends EventEmitter {
  constructor(debug) {
    super()
    this.debug = debug
  }
  gotMessage(sendCallback, message) {
    if (!message.SB) return
    if (this.debug)
      console.debug(message)

    if (message.Genre === 'send') {
      message.Response = null
      let response
      try  {
        this.emit(message.Type, message)
        response = message.Response instanceof Promise ? message.Response : Promise.resolve(message.Response)
      } catch (err) {
        response = Promise.reject(err)
      }
      response.then(retVal => {
        sendCallback({Genre: 'response', Status: true, Result: retVal, ID: message.ID, SB: true})
      }, retVal => {
        if (retVal instanceof Error) {
          const error = {__sb_is_error: true}
          Object.getOwnPropertyNames(retVal).forEach(function(key){
            error[key] = retVal[key]
          })
          retVal = error
        }
        sendCallback({Genre: 'response', Status: false, Result: retVal, ID: message.ID, SB: true})
      })
    } else if(message.Genre === 'response') {
      if (message.Result && typeof message.Result === 'object' && message.Result.__sb_is_error) {
        const error = new Error()
        for (var key in message.Result) {
          if (key !== '__sb_is_error')
            error[key] = message.Result[key]
        }
        message.Result = error
      }
      this.emit(`JOB:${message.ID}`, message)
    }
  }
  request(sendCallback, type, message) {
    return new Promise((resolve, reject) => {
      const JobID = Communication.randomId()
      var disposable = this.on(`JOB:${JobID}`, function(Message){
        disposable.dispose()
        if (Message.Status) resolve(Message.Result)
        else reject(Message.Result)
      })
      sendCallback({Type: type, Genre: 'send', Message: message, SB : true, ID: JobID})
    })
  }
  static randomId() {
    return (Math.random().toString(36)+'00000000000000000').slice(2, 7+2)
  }
}
module.exports = Communication

package com.petuxbot.services

import canoe.api.models.Keyboard
import canoe.models.{KeyboardButton, ReplyKeyboardMarkup}

object KeyboardService {
  def create(buttons: String*): Keyboard.Reply = {
    val keyboardButtons = buttons.map(button => KeyboardButton.text(button))
    val inlineKeyboardMarkUp: ReplyKeyboardMarkup = ReplyKeyboardMarkup.singleRow(keyboardButtons)
    Keyboard.Reply(inlineKeyboardMarkUp)
  }

  def createFromList(buttons: List[String]): Keyboard.Reply = {
    val keyboardButtons = buttons.map(button => KeyboardButton.text(button))
    val inlineKeyboardMarkUp: ReplyKeyboardMarkup = ReplyKeyboardMarkup.singleColumn(keyboardButtons)
    Keyboard.Reply(inlineKeyboardMarkUp)
  }

}

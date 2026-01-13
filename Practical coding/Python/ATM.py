import random
import re


class ATM:
    def __init__(self, username, bank, amount):
        self.username = username
        self.bank = bank
        self.amount = amount

    def deposit(self, saving):
        self.amount += saving
        print(f"Deposit {saving} THB Successful, remaining {self.amount} THB")

    def withdraw(self, paying):
        self.amount -= paying
        print(f"withdraw {paying} THB Successful, remaining {self.amount} THB")

    def check_balance(self):
        print(f"Balance : {self.amount} THB")

    def OTP(self):
       while True:
          phone = input("What your number: ").strip()
          if len(phone) == 10 and phone.startswith('0') and phone.isdigit():
              otp_rand = random.randint(100000, 999999)
              print(f"OTP is {otp_rand} already sent to your number, {phone}")
              break
          else:
              print(f"Please check your phone number again")

    def order_coffee(self):
        total = 0
        bill = []
        menus = {
            "Americano": 55,
            "Latte": 60,
            "Cappuccino": 65,
            "Espresso": 60,
            "Coco": 60
            }

        print(f"{'Menu':<15} {'Price'}")
        print("-" * 22) # Divider line

        for item, price in menus.items():
          print(f"{item:<15} {price} THB")

        lowered_menus = {item.lower(): (item, price) for item, price in menus.items()}

        while True:
          order = input("What would you like to order? or check bill?: ").strip()

          if order == "check bill":
            print("\n--- Order Summary ---")
            if self.amount >= total:
              self.amount -= total
              for index, (name, qty, sub) in enumerate(bill):
                print(f"{index + 1}: {name} x{qty} = {sub} THB")

              print("-" * 22)
              print(f"Totally {total} THB")
              print(f"Payment Successful! Remaining Balance: {self.amount} THB")
              print("Thank you very much")
              break
            else:
              print("Your account not enough money")
              print("Please order again or change your items.")

          elif order.lower() in lowered_menus:
            count = int(input(f"How many {order} would you like? "))

            # ดึงชื่อเมนูที่ถูกต้องและราคามาจาก Dictionary
            original_name, price = lowered_menus[order.lower()]

            subtotal = price * count
            total += subtotal

            current_order = (original_name, count, subtotal)
            bill.append(current_order)

            print(f"Added {count} {original_name}(s): {subtotal} THB")
            print(f"Current Total: {total} THB")

          else:
            print("Sorry, we don't have that menu. Please select a new one.")

    def bill(self):
      menus = {
          "water bill": "1",
          "electricity bill": "2",
          "internet bill": "3"}
      print(f"{'Menu':<15} {'choice'}")
      print("-" * 22) # Divider line

      for item, choice in menus.items():
        print(f"{item:<17} {choice}")

      while True:
        select = float(input("What would you want to bill: "))
        if select == 1:
          water_pay = random.randint(100,400)
          if self.amount >= water_pay:
            self.amount -= water_pay
            print(f"Your water bill {water_pay} BTH, Remaining Balance: {self.amount} THB")
          else:
            print(f"Please deposit to account")
          break
        elif select == 2:
          electic_pay = random.randint(500,999)
          if self.amount >= electic_pay:
            self.amount -= electic_pay
            print(f"Your electricity bill {electic_pay} BTH, Remaining Balance: {self.amount} THB")
          else:
            print(f"Please deposit to account")
          break
        elif select == 3:
          internet_pay = random.randint(1000,2000)
          if self.amount >= internet_pay:
            self.amount -= internet_pay
            print(f"Your internet bill {internet_pay} BTH, Remaining Balance: {self.amount} THB")
          else:
            print(f"Please deposit to account")
          break
